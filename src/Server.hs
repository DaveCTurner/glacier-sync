{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Server where

import           API
import           API.Types
import           Context
import           Control.Concurrent.STM
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
import           ServantUtils
import           StoredCredentials

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

