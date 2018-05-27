{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Server where

import           API
import           API.Types
import           Context
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens                   hiding (Context, (.=))
import           Control.Monad.Except
import           Control.Timeout
import           Data.Aeson
import qualified Data.ByteString.Lazy           as BL
import           Data.Proxy
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Time
import           Data.Time.ISO8601
import           Network.AWS
import           Network.AWS.Env                (newEnvWith)
import           Network.AWS.Glacier.ListVaults
import           Network.AWS.STS.AssumeRole
import           Network.HTTP.Client            hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header
import           Network.Wai
import           Servant                        hiding (Context)
import           StoredCredentials
import           System.FilePath                ((</>))
import           System.IO                      (IOMode (WriteMode), withFile)

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
    void $ updateEnv Nothing
    return NoContent

  serveAwsSetSecurityConfigAPI :: Server AwsSetSecurityConfigAPI
  serveAwsSetSecurityConfigAPI AwsSetSecurityConfigRequest{..} = liftIO $ do
    atomically $ modifyTVar' (awsConfigVar context) (\awsConfig -> awsConfig
      { awsMfaSerial = awsSetSecurityConfigRequestMfaSerialNumber
      , awsRoleArn   = awsSetSecurityConfigRequestRoleArn
      })
    void $ updateEnv Nothing
    return NoContent

  serveAwsSetMFACodeAPI :: Server AwsSetMFACodeAPI
  serveAwsSetMFACodeAPI AwsSetMFACodeRequest{..} = do
    AwsConfig
      { awsAccessKey =           currentAccessKey
      , awsSecretKey = Redacted  currentSecretKey
      , awsRoleArn   = RoleArn   currentRoleArn
      , awsMfaSerial = MfaSerial currentMfaSerial
      } <- liftIO $ readTVarIO (awsConfigVar context)

    when (currentAccessKey == AccessKey "" || currentSecretKey == SecretKey "")
      $ throwErrorString "AWS account credentials are not set"

    when (currentRoleArn == "" || currentMfaSerial == "")
      $ throwErrorString "AWS session details are not set"

    httpManager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }

    errorOrAssumeRoleResponse <- liftIO $ do

      env <- newEnvWith (FromKeys currentAccessKey currentSecretKey) (Just False) httpManager

      let MfaCode rawMfaCode = awsSetMFACodeRequestMfaCode

      runResourceT $ runAWS env $ trying _Error $ send $ assumeRole currentRoleArn "glacier-sync-session-name"
        & arTokenCode    .~ Just rawMfaCode
        & arSerialNumber .~ Just currentMfaSerial

    liftIO $ print errorOrAssumeRoleResponse

    let liftMaybe message = maybe (throwErrorString message) return

    assumeRoleResult <- either throwErrorShow return errorOrAssumeRoleResponse
    authEnv <- liftMaybe "no credentials returned" $ assumeRoleResult ^. arrsCredentials
    let newSessionAccessKey = authEnv ^. accessKeyId
        newSessionSecretKey = authEnv ^. secretAccessKey
    newSessionToken  <- liftMaybe "no session token returned"  $ authEnv ^. sessionToken
    newSessionExpiry <- addUTCTime (-600) -- destroy it 10 minutes before expiry
                    <$> liftMaybe "no session expiry returned" (authEnv ^. expiration)

    sessionEnv <- liftIO $ newEnvWith (FromSession newSessionAccessKey newSessionSecretKey newSessionToken) (Just False) httpManager

    -- verify env can list vaults
    either throwErrorShow (const $ return ()) =<< liftIO (runResourceT $ runAWS sessionEnv $ within Ireland $ trying _Error $ send $ listVaults "-" & lvLimit .~ Just "10")

    newVersion <- liftMaybe "impossible? environment not updated" =<< updateEnv (Just sessionEnv)
    void $ liftIO $ forkIO $ scheduleEnvDestruction newVersion newSessionExpiry

    -- save env details for later
    liftIO $ withFile (configPath context </> "credentials.json") WriteMode $ \h -> BL.hPutStr h $ encode $ StoredCredentials
      { storedCredentialsAccessKey        =           currentAccessKey
      , storedCredentialsSecretKey        = Redacted  currentSecretKey
      , storedCredentialsRoleArn          = RoleArn   currentRoleArn
      , storedCredentialsMfaSerial        = MfaSerial currentMfaSerial
      , storedCredentialsSessionAccessKey =           newSessionAccessKey
      , storedCredentialsSessionSecretKey = Redacted  newSessionSecretKey
      , storedCredentialsSessionToken     = Redacted  newSessionToken
      , storedCredentialsSessionExpiry    =           newSessionExpiry
      }

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

