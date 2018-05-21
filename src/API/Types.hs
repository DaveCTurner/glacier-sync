{-# LANGUAGE OverloadedStrings #-}

module API.Types where

import qualified Data.Text as T
import Data.Aeson

data AwsSetCredentialsRequest = AwsSetCredentialsRequest
  { credsAccessKey :: T.Text
  , credsSecretKey :: T.Text
  } deriving (Show, Eq) 

instance FromJSON AwsSetCredentialsRequest where
  parseJSON = withObject "AwsSetCredentialsRequest" $ \v ->
    AwsSetCredentialsRequest <$> v .: "access_key" <*> v .: "secret_key"

data AwsSetSecurityConfigRequest = AwsSetSecurityConfigRequest
  { secConfMfaSerialNumber :: T.Text
  , secConfRoleARN         :: T.Text
  } deriving (Show, Eq)

instance FromJSON AwsSetSecurityConfigRequest where
  parseJSON = withObject "AwsSetSecurityConfigRequest" $ \v ->
    AwsSetSecurityConfigRequest <$> v .: "mfa_serial" <*> v .: "role_arn"
