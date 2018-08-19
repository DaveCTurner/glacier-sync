{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module API.Types where

import           Data.Aeson
import qualified Data.Text        as T
import           Data.Time
import           Network.AWS.Auth

newtype Redacted a = Redacted a deriving (Eq, FromJSON, ToJSON)

instance Show (Redacted a) where show _ = "<REDACTED>"

newtype MfaSerial = MfaSerial T.Text deriving (Show, Eq, ToJSON, FromJSON)
newtype RoleArn   = RoleArn   T.Text deriving (Show, Eq, ToJSON, FromJSON)
newtype MfaCode   = MfaCode   T.Text deriving (Show, Eq, ToJSON, FromJSON)

data AwsSetCredentialsRequest = AwsSetCredentialsRequest
  { awsSetCredentialsRequestAccessKey :: AccessKey
  , awsSetCredentialsRequestSecretKey :: Redacted SecretKey
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

data AwsGetStatusResponse = AwsGetStatusResponse
  { awsGetStatusResponseMfaSerial          :: MfaSerial
  , awsGetStatusResponseRoleArn            :: RoleArn
  , awsGetStatusResponseEnvironmentVersion :: Int
  , awsGetStatusSessionExpiry              :: Maybe UTCTime
  } deriving (Show, Eq)

instance ToJSON AwsGetStatusResponse where
  toJSON AwsGetStatusResponse{..} = object
    [ "mfa_serial"          .= awsGetStatusResponseMfaSerial
    , "role_arn"            .= awsGetStatusResponseRoleArn
    , "environment_version" .= awsGetStatusResponseEnvironmentVersion
    , "session" .= object (case awsGetStatusSessionExpiry of
        Nothing -> ["open" .= False]
        Just e  -> ["open" .= True, "expiry" .= e])
    ]

data StartUploadRequest = StartUploadRequest
  { startUploadRequestMirrorPath :: T.Text -- TODO this should be in config
  , startUploadRequestVaultName  :: T.Text
  , startUploadRequestPath       :: T.Text
  } deriving (Show, Eq)

instance FromJSON StartUploadRequest where
  parseJSON = withObject "StartUploadRequest" $ \v ->
    StartUploadRequest
      <$> v .: "mirror_path"
      <*> v .: "vault_name"
      <*> v .: "path"
