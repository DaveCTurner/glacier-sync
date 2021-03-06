{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Server where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception          (mask, onException)
import           Control.Lens               hiding (Context, (.=))
import           Control.Monad.Except
import           Data.Proxy
import           Data.Time
import           Network.AWS
import           Network.AWS.Env            (newEnvWith)
import           Network.AWS.STS.AssumeRole
import           Network.HTTP.Client        hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Network.Wai
import           Servant                    hiding (Context)

import           API
import           API.Types
import           Context
import           LocalInventory
import           ServantUtils
import           StoredCredentials
import           Task
import           Upload                     (upload)

application :: Context -> Application
application context = serve (Proxy :: Proxy API) serveAPI where

  serveAPI :: Server API
  serveAPI = serveSecurityAPI :<|> serveUploadAPI :<|> serveTasksAPI :<|> serveLocalInventoryAPI

  serveSecurityAPI :: Server SecurityAPI
  serveSecurityAPI = serveSecurityAwsAPI

  serveSecurityAwsAPI :: Server SecurityAwsAPI
  serveSecurityAwsAPI = serveAwsSetCredentialsAPI
                   :<|> serveAwsSetSecurityConfigAPI
                   :<|> serveAwsSetMFACodeAPI
                   :<|> serveAwsGetStatusAPI

  serveAwsGetStatusAPI :: Server AwsGetStatusAPI
  serveAwsGetStatusAPI = do
    AwsConfig
      { awsRoleArn   = awsGetStatusResponseRoleArn
      , awsMfaSerial = awsGetStatusResponseMfaSerial
      } <- liftIO $ readTVarIO $ awsConfigVar context
    VersionedEnv
      { veVersion = awsGetStatusResponseEnvironmentVersion
      , veEnv     = maybeEnvExpiry
      } <- liftIO $ readTVarIO $ awsEnvVar context
    return $ AwsGetStatusResponse
      { awsGetStatusSessionExpiry = snd <$> maybeEnvExpiry
      , .. }

  serveAwsSetCredentialsAPI :: Server AwsSetCredentialsAPI
  serveAwsSetCredentialsAPI AwsSetCredentialsRequest{..} = liftIO $ atomically $ do
    modifyTVar' (awsConfigVar context) (\awsConfig -> awsConfig
      { awsAccessKey = awsSetCredentialsRequestAccessKey
      , awsSecretKey = awsSetCredentialsRequestSecretKey
      })
    return NoContent

  serveAwsSetSecurityConfigAPI :: Server AwsSetSecurityConfigAPI
  serveAwsSetSecurityConfigAPI AwsSetSecurityConfigRequest{..} = liftIO $ atomically $ do
    modifyTVar' (awsConfigVar context)
      (\awsConfig -> awsConfig
        { awsMfaSerial = awsSetSecurityConfigRequestMfaSerialNumber
        , awsRoleArn   = awsSetSecurityConfigRequestRoleArn
        })
    return NoContent

  serveAwsSetMFACodeAPI :: Server AwsSetMFACodeAPI
  serveAwsSetMFACodeAPI AwsSetMFACodeRequest{awsSetMFACodeRequestMfaCode = MfaCode rawMfaCode} = do
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

    assumeRoleResult <- either throwErrorShow return =<< liftIO (do

      env <- newEnvWith (FromKeys currentAccessKey currentSecretKey) (Just False) httpManager

      runResourceT $ runAWS env $ trying _Error $ send $ assumeRole currentRoleArn "glacier-sync-session-name"
        & arTokenCode    .~ Just rawMfaCode
        & arSerialNumber .~ Just currentMfaSerial)

    authEnv <- liftMaybe "no credentials returned" $ assumeRoleResult ^. arrsCredentials
    let newSessionAccessKey = authEnv ^. accessKeyId
        newSessionSecretKey = authEnv ^. secretAccessKey
    newSessionToken  <- liftMaybe "no session token returned"  $ authEnv ^. sessionToken
    newSessionExpiry <- addUTCTime (-600) -- destroy it 10 minutes before expiry
                    <$> liftMaybe "no session expiry returned" (authEnv ^. expiration)

    createAndValidateEnv context StoredCredentials
      { storedCredentialsAccessKey =           currentAccessKey
      , storedCredentialsSecretKey = Redacted  currentSecretKey
      , storedCredentialsRoleArn   = RoleArn   currentRoleArn
      , storedCredentialsMfaSerial = MfaSerial currentMfaSerial
      , storedCredentialsSession   = StoredSessionCredentials
        { storedSessionCredentialsAccessKey =          newSessionAccessKey
        , storedSessionCredentialsSecretKey = Redacted newSessionSecretKey
        , storedSessionCredentialsToken     = Redacted newSessionToken
        , storedSessionCredentialsExpiry    =          newSessionExpiry
        }
      }

    return NoContent

  serveUploadAPI :: Server UploadAPI
  serveUploadAPI startUploadRequest =
    liftIO $ forkTask (ctxTaskManager context) startUploadRequest ("starting" :: String) $ upload context startUploadRequest

  serveTasksAPI :: Server TasksAPI
  serveTasksAPI = serveTasksList :<|> serveTaskAPI

  serveTasksList :: Handler [Task]
  serveTasksList = liftIO $ atomically $ getTasks $ ctxTaskManager context

  serveTaskAPI :: Integer -> Server TaskAPI
  serveTaskAPI taskId = serveGetTask :<|> serveDeleteTask
    where
    taskNotFound = throwError err404

    serveGetTask :: Handler TaskStatus
    serveGetTask = maybe taskNotFound return =<< liftIO (atomically $ getTaskStatus (ctxTaskManager context) taskId)

    serveDeleteTask :: Handler NoContent
    serveDeleteTask = do
      success <- liftIO $ cancelTask (ctxTaskManager context) taskId
      if success then return NoContent else taskNotFound

  serveLocalInventoryAPI :: Server LocalInventoryAPI
  serveLocalInventoryAPI = serveGetLocalInventory :<|> serveRefreshLocalInventory :<|> serveRebuildLocalInventory

  serveGetLocalInventory :: Handler LocalInventory
  serveGetLocalInventory = do
    resultVar <- liftIO newEmptyMVar
    void $ liftIO $ mask $ \restore ->
      forkTask (ctxTaskManager context) ("get-inventory" :: String) ("starting" :: String) $ \taskInner -> do
        restore (getLocalInventory taskInner $ putMVar resultVar . Just) `onException` putMVar resultVar Nothing
    maybe (throwErrorString "get-inventory failed") return =<< liftIO (readMVar resultVar)

  serveRefreshLocalInventory :: Handler Task
  serveRefreshLocalInventory
    = liftIO $ forkTask (ctxTaskManager context) ("refresh-inventory" :: String) ("starting" :: String)
                        (refreshLocalInventory context)

  serveRebuildLocalInventory :: Handler Task
  serveRebuildLocalInventory
    = liftIO $ forkTask (ctxTaskManager context) ("rebuild-inventory" :: String) ("starting" :: String)
                        (rebuildLocalInventory context)
