{-# LANGUAGE OverloadedStrings #-}

module Context where

import API.Types
import Network.AWS
import Network.AWS.Env
import Network.AWS.Auth
import Control.Concurrent.STM.TVar

data AwsConfig = AwsConfig
  { awsAccessKey :: !AccessKey
  , awsSecretKey :: !RedactedSecretKey
  , awsRoleArn   :: !RoleArn
  , awsMfaSerial :: !MfaSerial
  } deriving (Show, Eq)

emptyAwsConfig :: AwsConfig
emptyAwsConfig = AwsConfig
  { awsAccessKey = AccessKey         ""
  , awsSecretKey = RedactedSecretKey ""
  , awsRoleArn   = RoleArn           ""
  , awsMfaSerial = MfaSerial         ""
  }

data VersionedEnv = VersionedEnv
  { veVersion :: !Int
  , veEnv     :: (Maybe Env)
  }

data Context = Context
  { awsConfigVar :: TVar AwsConfig
  , awsEnvVar    :: TVar VersionedEnv
  }

makeEmptyContext :: IO Context
makeEmptyContext = Context
    <$> newTVarIO emptyAwsConfig
    <*> newTVarIO (VersionedEnv 0 Nothing)
