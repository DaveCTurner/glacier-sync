{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module StoredCredentials where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens                   hiding (Context, (.=))
import           Control.Monad.Except
import           Control.Timeout
import           Data.Aeson
import qualified Data.ByteString.Lazy           as BL
import           Data.Time
import           Data.Time.ISO8601
import           Network.AWS
import           Network.AWS.Env                (newEnvWith)
import           Network.AWS.Glacier.ListVaults
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Servant                        hiding (Context)
import           System.IO                      (IOMode (WriteMode), withFile)

import           API.Types
import           Context
import           ServantUtils

data StoredSessionCredentials = StoredSessionCredentials
  { storedSessionCredentialsAccessKey :: AccessKey
  , storedSessionCredentialsSecretKey :: Redacted SecretKey
  , storedSessionCredentialsToken     :: Redacted SessionToken
  , storedSessionCredentialsExpiry    :: UTCTime
  } deriving (Show, Eq)

instance ToJSON StoredSessionCredentials where
  toJSON StoredSessionCredentials{..} = object
    [ "access_key"    .= storedSessionCredentialsAccessKey
    , "secret_key"    .= storedSessionCredentialsSecretKey
    , "session_token" .= storedSessionCredentialsToken
    , "expiry"        .= storedSessionCredentialsExpiry
    ]

instance FromJSON StoredSessionCredentials where
  parseJSON = withObject "StoredSessionCredentials" $ \o -> StoredSessionCredentials
    <$> o .: "access_key"
    <*> o .: "secret_key"
    <*> o .: "session_token"
    <*> o .: "expiry"

data StoredCredentials = StoredCredentials
  { storedCredentialsAccessKey :: AccessKey
  , storedCredentialsSecretKey :: Redacted SecretKey
  , storedCredentialsRoleArn   :: RoleArn
  , storedCredentialsMfaSerial :: MfaSerial
  , storedCredentialsSession   :: StoredSessionCredentials
  } deriving (Show, Eq)

instance ToJSON StoredCredentials where
  toJSON StoredCredentials{..} = object
    [ "access_key" .= storedCredentialsAccessKey
    , "secret_key" .= storedCredentialsSecretKey
    , "role_arn"   .= storedCredentialsRoleArn
    , "mfa_serial" .= storedCredentialsMfaSerial
    , "session"    .= storedCredentialsSession
    ]

instance FromJSON StoredCredentials where
  parseJSON = withObject "StoredCredentials" $ \o -> StoredCredentials
    <$> o .: "access_key"
    <*> o .: "secret_key"
    <*> o .: "role_arn"
    <*> o .: "mfa_serial"
    <*> o .: "session"


authFromStoredCredentials :: StoredCredentials -> Credentials
authFromStoredCredentials = authFromStoredSessionCredentials . storedCredentialsSession

authFromStoredSessionCredentials :: StoredSessionCredentials -> Credentials
authFromStoredSessionCredentials StoredSessionCredentials
  { storedSessionCredentialsAccessKey =          accessKey
  , storedSessionCredentialsSecretKey = Redacted secretKey
  , storedSessionCredentialsToken     = Redacted token
  } = FromSession accessKey secretKey token

createAndValidateEnv :: (MonadIO m, MonadError ServantErr m) => Context -> StoredCredentials -> m ()
createAndValidateEnv context storedCredentials = do

  sessionEnv <- liftIO $ newEnvWith (authFromStoredCredentials storedCredentials) (Just False)
                     =<< newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }

  -- verify env can list vaults
  either throwErrorShow (const $ return ()) =<< liftIO (runResourceT $ runAWS sessionEnv $ within Ireland $ trying _Error $ send $ listVaults "-" & lvLimit .~ Just "10")

  let sessionExpiry = storedSessionCredentialsExpiry $ storedCredentialsSession storedCredentials
  newVersion <- liftMaybe "impossible? environment not updated" =<< updateEnv (awsEnvVar context) (Just (sessionEnv, sessionExpiry))
  void $ liftIO $ forkIO $ scheduleEnvDestruction (awsEnvVar context) newVersion sessionExpiry

  -- save env details for later
  liftIO $ withFile (credentialsFile context) WriteMode $ \h -> BL.hPutStr h $ encode storedCredentials

scheduleEnvDestruction :: TVar VersionedEnv -> Int -> UTCTime -> IO ()
scheduleEnvDestruction envVar versionToDestroy expiryTime = do
    putStrLn $ "scheduling destruction of environment version " ++ show versionToDestroy ++ " at " ++ formatISO8601Millis expiryTime
    destroyOrRetry
  where
    destroyOrRetry = do
      now <- getCurrentTime
      if now < expiryTime then do
        sleep (diffUTCTime expiryTime now + 1)
        destroyOrRetry
      else void $ join $ atomically $ do
        currentVersion <- veVersion <$> readTVar envVar
        if currentVersion == versionToDestroy
          then updateEnvSTM envVar Nothing
          else return $ return Nothing

updateEnvSTM :: TVar VersionedEnv -> Maybe (Env, UTCTime) -> STM (IO (Maybe Int))
updateEnvSTM envVar maybeEnv = do
  oldVersionedEnv <- readTVar envVar
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
      writeTVar envVar $! VersionedEnv newEnvVersion maybeEnv
      return $ do
        putStrLn $ "updateEnv: " ++ case change of
          CreateNewEnv  -> "created new environment with version " ++ show newEnvVersion
          DestroyOldEnv -> "clearing environment version " ++ show oldEnvVersion
          ReplaceEnv    -> "replacing environment version " ++ show oldEnvVersion ++ " with version " ++ show newEnvVersion
        return $ Just newEnvVersion

updateEnv :: MonadIO m => TVar VersionedEnv -> Maybe (Env, UTCTime) -> m (Maybe Int)
updateEnv envVar = liftIO . join . atomically . updateEnvSTM envVar

data UpdateEnvChange = CreateNewEnv | DestroyOldEnv | ReplaceEnv

loadStoredCredentialsIfAvailable :: Context -> IO ()
loadStoredCredentialsIfAvailable context = tryLoad `catch` ignoreIOException
  where
  ignoreIOException :: IOException -> IO ()
  ignoreIOException _ = return ()

  tryLoad = do
    bytes <- BL.readFile (credentialsFile context)
    void $ runExceptT $ do
      credentials <- liftMaybe "JSON parse error" $ decode bytes
      liftIO $ atomically $ writeTVar (awsConfigVar context) $ AwsConfig
        { awsAccessKey = storedCredentialsAccessKey credentials
        , awsSecretKey = storedCredentialsSecretKey credentials
        , awsRoleArn   = storedCredentialsRoleArn   credentials
        , awsMfaSerial = storedCredentialsMfaSerial credentials
        }
      createAndValidateEnv context credentials
