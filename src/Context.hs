{-# LANGUAGE OverloadedStrings #-}

module Context where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.Text               as T
import           Data.Time
import           Network.AWS
import           System.Environment      (getEnv)
import           System.FilePath         (FilePath, (</>))

import           API.Types
import           Config
import           Task

data AwsConfig = AwsConfig
  { awsAccessKey :: !AccessKey
  , awsSecretKey :: !(Redacted SecretKey)
  , awsRoleArn   :: !RoleArn
  , awsMfaSerial :: !MfaSerial
  } deriving (Show, Eq)

emptyAwsConfig :: AwsConfig
emptyAwsConfig = AwsConfig
  { awsAccessKey = AccessKey ""
  , awsSecretKey = Redacted  (SecretKey "")
  , awsRoleArn   = RoleArn   ""
  , awsMfaSerial = MfaSerial ""
  }

data VersionedEnv = VersionedEnv
  { veVersion :: !Int
  , veEnv     :: (Maybe (Env, UTCTime))
  }

data Context = Context
  { awsConfigVar     :: TVar AwsConfig
  , awsEnvVar        :: TVar VersionedEnv
  , configPath       :: FilePath
  , ctxTaskManager   :: TaskManager
  , ctxUploaderSlots :: TVar Int
  , ctxConfigVar     :: MVar Config
  }

makeEmptyContext :: IO Context
makeEmptyContext = do
  configDir <- (</> ".glacier-sync") <$> getEnv "HOME"
  config    <- loadConfig $ configDir </> "config.json"

  Context
    <$> newTVarIO emptyAwsConfig
    <*> newTVarIO (VersionedEnv 0 Nothing)
    <*> pure configDir
    <*> newTaskManager
    <*> newTVarIO 1
    <*> newMVar config

credentialsFile :: Context -> FilePath
credentialsFile = (</> "credentials.json") . configPath

configFile :: Context -> FilePath
configFile = (</> "config.json") . configPath

getAwsEnv :: Context -> IO Env
getAwsEnv context = atomically $ maybe retry (return . fst) <$> veEnv =<< readTVar (awsEnvVar context)

withUploaderSlot :: Context -> TaskInner -> IO () -> IO ()
withUploaderSlot context taskInner go = mask $ \restore -> do
  void $ atomically $ taskSetStatus taskInner ("awaiting uploader slot" :: T.Text)
  gotSlot <- atomically $ consumeSlot `orElse` awaitCancellation
  finally (when gotSlot $ restore go) (when gotSlot $ atomically releaseSlot)

  where
  consumeSlot = do
    n <- readTVar $ ctxUploaderSlots context
    guard $ 0 < n
    writeTVar (ctxUploaderSlots context) $! n - 1
    return True

  awaitCancellation = do
    status <- taskSetStatus taskInner ("cancelled awaiting uploader slot" :: T.Text)
    case status of
      Nothing            -> retry
      Just TaskCancelled -> return False

  releaseSlot = do
    modifyTVar' (ctxUploaderSlots context) (+1)

