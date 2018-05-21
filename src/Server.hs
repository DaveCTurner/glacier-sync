{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import API
import API.Types
import Servant hiding (Context)
import Network.Wai
import Data.Proxy
import Control.Monad.IO.Class
import Context
import Control.Concurrent.STM
import Network.AWS
import Network.AWS.Env
import Network.AWS.STS.AssumeRole
import Control.Lens hiding (Context)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Header
import Network.HTTP.Client.TLS
import Control.Monad.Except
import Data.Either.Combinators
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Control.Timeout
import Control.Concurrent
import Data.Time
import Data.Time.ISO8601

throwErrorShow :: Show e => e -> Handler a
throwErrorShow = throwErrorString . show

throwErrorString :: String -> Handler a
throwErrorString message = throwError $ ServantErr
  { errHTTPCode = 500
  , errReasonPhrase = "Internal error"
  , errBody = BL.fromStrict $ T.encodeUtf8 $ T.pack message
  , errHeaders = [(hContentType, "text/plain; encoding=utf-8")]
  }

application :: Context -> Application
application context = serve (Proxy :: Proxy API) serveAPI where

  serveAPI :: Server API
  serveAPI = serveSecurityAPI

  serveSecurityAPI :: Server SecurityAPI
  serveSecurityAPI = serveSecurityAwsAPI

  serveSecurityAwsAPI :: Server SecurityAwsAPI
  serveSecurityAwsAPI = serveAwsSetCredentialsAPI
                   :<|> serveAwsSetSecurityConfigAPI
                   :<|> serveAwsSetMFACodeAPI

  serveAwsSetCredentialsAPI :: Server AwsSetCredentialsAPI
  serveAwsSetCredentialsAPI AwsSetCredentialsRequest{..} = liftIO $ do
    atomically $ modifyTVar' (awsConfigVar context) (\awsConfig -> awsConfig
      { awsAccessKey = awsSetCredentialsRequestAccessKey
      , awsSecretKey = awsSetCredentialsRequestSecretKey
      })
    updateEnv Nothing
    return NoContent

  serveAwsSetSecurityConfigAPI :: Server AwsSetSecurityConfigAPI
  serveAwsSetSecurityConfigAPI AwsSetSecurityConfigRequest{..} = liftIO $ do
    atomically $ modifyTVar' (awsConfigVar context) (\awsConfig -> awsConfig
      { awsMfaSerial = awsSetSecurityConfigRequestMfaSerialNumber
      , awsRoleArn   = awsSetSecurityConfigRequestRoleArn
      })
    updateEnv Nothing
    return NoContent

  serveAwsSetMFACodeAPI :: Server AwsSetMFACodeAPI
  serveAwsSetMFACodeAPI AwsSetMFACodeRequest{..} = do
    AwsConfig
      { awsAccessKey = currentAccessKey
      , awsSecretKey = RedactedSecretKey currentSecretKey
      , awsRoleArn   = RoleArn           currentRoleArn
      , awsMfaSerial = MfaSerial         currentMfaSerial
      } <- liftIO $ readTVarIO (awsConfigVar context)

    when (currentAccessKey == AccessKey "" || currentSecretKey == SecretKey "")
      $ throwErrorString "AWS account credentials are not set"

    when (currentRoleArn == "" || currentMfaSerial == "")
      $ throwErrorString "AWS session details are not set"

    httpManager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }

    errorOrAssumeRoleResponse <- liftIO $ do

      env <- newEnvWith (FromKeys currentAccessKey currentSecretKey) (Just False) httpManager

      let MfaCode rawMfaCode = awsSetMFACodeRequestMfaCode

      runResourceT $ runAWS env $ trying _Error $ send $ assumeRole currentRoleArn "glacier-sync"
        & arTokenCode    .~ Just rawMfaCode
        & arSerialNumber .~ Just currentMfaSerial

    liftIO $ print errorOrAssumeRoleResponse

    let liftMaybe message = maybe (throwErrorString message) return

    assumeRoleResponse <- either throwErrorShow return errorOrAssumeRoleResponse
    authEnv <- liftMaybe "no credentials returned" $ assumeRoleResponse ^. arrsCredentials
    let newSessionAccessKey = authEnv ^. accessKeyId
        newSessionSecretKey = authEnv ^. secretAccessKey
    newSessionToken  <- liftMaybe "no session token returned"  $ authEnv ^. sessionToken
    newSessionExpiry <- liftMaybe "no session expiry returned" $ authEnv ^. expiration

    maybeNewVersion <- updateEnv . Just =<< liftIO (newEnvWith (FromSession newSessionAccessKey newSessionSecretKey newSessionToken) (Just False) httpManager)
    newVersion <- liftMaybe "impossible? environment not updated" maybeNewVersion

    void $ liftIO $ forkIO $ scheduleEnvDestruction newVersion $ addUTCTime (-600) newSessionExpiry -- destroy it 10 minutes before expiry

    return NoContent

  scheduleEnvDestruction :: Int -> UTCTime -> IO ()
  scheduleEnvDestruction versionToDestroy expiryTime = do
      putStrLn $ "scheduling destruction of environment version " ++ show versionToDestroy ++ " at " ++ formatISO8601Millis expiryTime
      destroyOrRetry
    where
      destroyOrRetry = do
        now <- getCurrentTime
        if now < expiryTime then do
          sleep (diffUTCTime expiryTime now + 1)
          destroyOrRetry
        else void $ join $ atomically $ do
          currentVersion <- veVersion <$> readTVar (awsEnvVar context)
          if currentVersion == versionToDestroy
            then updateEnvSTM Nothing
            else return $ return Nothing

  updateEnvSTM :: Maybe Env -> STM (IO (Maybe Int))
  updateEnvSTM maybeEnv = do
    oldVersionedEnv <- readTVar $ awsEnvVar context
    let oldEnvVersion = veVersion oldVersionedEnv
        newEnvVersion = oldEnvVersion + 1
        maybeChange = case (veEnv oldVersionedEnv, maybeEnv) of
          (Nothing, Nothing) -> Nothing
          (Nothing, _)       -> Just CreateNewEnv
          (_, Nothing)       -> Just DestroyOldEnv
          (_, _)             -> Just ReplaceEnv

    case maybeChange of
      Nothing -> return $ do
        putStrLn $ "updateEnv: clearing environment, but environment version " ++ show oldEnvVersion ++ " is already unset"
        return Nothing

      Just change -> do
        writeTVar (awsEnvVar context) $! VersionedEnv newEnvVersion maybeEnv
        return $ do
          putStrLn $ "updateEnv: " ++ case change of
            CreateNewEnv  -> "created new environment with version " ++ show newEnvVersion
            DestroyOldEnv -> "clearing environment version " ++ show oldEnvVersion
            ReplaceEnv    -> "replacing environment version " ++ show oldEnvVersion ++ " with version " ++ show newEnvVersion
          return $ Just newEnvVersion

  updateEnv :: MonadIO m => Maybe Env -> m (Maybe Int)
  updateEnv = liftIO . join . atomically . updateEnvSTM

data UpdateEnvChange = CreateNewEnv | DestroyOldEnv | ReplaceEnv
