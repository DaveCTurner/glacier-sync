{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module API.Types where

import qualified Data.Text as T
import Data.Aeson
import Data.String
import Network.AWS.Auth

newtype RedactedSecretKey = RedactedSecretKey SecretKey deriving (Eq, FromJSON)
instance Show RedactedSecretKey where show _ = "<REDACTED>"

newtype MfaSerial = MfaSerial T.Text deriving (Show, Eq, FromJSON)
newtype RoleArn   = RoleArn   T.Text deriving (Show, Eq, FromJSON)
newtype MfaCode   = MfaCode   T.Text deriving (Show, Eq, FromJSON)

data AwsSetCredentialsRequest = AwsSetCredentialsRequest
  { awsSetCredentialsRequestAccessKey :: AccessKey
  , awsSetCredentialsRequestSecretKey :: RedactedSecretKey
  } deriving (Show, Eq) 

instance FromJSON AwsSetCredentialsRequest where
  parseJSON = withObject "AwsSetCredentialsRequest" $ \v ->
    AwsSetCredentialsRequest <$> v .: "access_key" <*> v .: "secret_key"

data AwsSetSecurityConfigRequest = AwsSetSecurityConfigRequest
  { awsSetSecurityConfigRequestMfaSerialNumber :: MfaSerial
  , awsSetSecurityConfigRequestRoleArn         :: RoleArn
  } deriving (Show, Eq)

instance FromJSON AwsSetSecurityConfigRequest where
  parseJSON = withObject "AwsSetSecurityConfigRequest" $ \v ->
    AwsSetSecurityConfigRequest <$> v .: "mfa_serial" <*> v .: "role_arn"

data AwsSetMFACodeRequest = AwsSetMFACodeRequest
  { awsSetMFACodeRequestMfaCode :: MfaCode
  } deriving (Show, Eq)

instance FromJSON AwsSetMFACodeRequest where
  parseJSON = withObject "AwsSetMFACodeRequest" $ \v ->
    AwsSetMFACodeRequest <$> v .: "mfa_code"
