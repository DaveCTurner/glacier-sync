{-# LANGUAGE OverloadedStrings #-}

module Context where

import           API.Types
import           Control.Concurrent.STM.TVar
import           Network.AWS
import           System.Environment          (getEnv)
import           System.FilePath             (FilePath, (</>))

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
  , veEnv     :: (Maybe Env)
  }

data Context = Context
  { awsConfigVar :: TVar AwsConfig
  , awsEnvVar    :: TVar VersionedEnv
  , configPath   :: FilePath
  }

makeEmptyContext :: IO Context
makeEmptyContext = Context
    <$> newTVarIO emptyAwsConfig
    <*> newTVarIO (VersionedEnv 0 Nothing)
    <*> ((</> ".glacier-sync") <$> getEnv "HOME")
