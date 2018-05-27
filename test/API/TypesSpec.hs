{-# LANGUAGE OverloadedStrings #-}

module API.TypesSpec where

import           API.Types

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Network.AWS.Auth
import           Test.Hspec

shouldParseAs :: (FromJSON a, Show a, Eq a) => BL.ByteString -> a -> Spec
shouldParseAs input expected
  = it ("decodes " ++ show input) $ eitherDecode input `shouldBe` Right expected

spec :: Spec
spec = describe "API.TypesSpec" $ do
  describe "AwsSetCredentialsRequest" $ do
    "{\"access_key\":\"ak\",\"secret_key\":\"sk\"}" `shouldParseAs`
      AwsSetCredentialsRequest (AccessKey "ak") (RedactedSecretKey "sk")

  describe "AwsSetSecurityConfigRequest" $ do
    "{\"mfa_serial\":\"abc\",\"role_arn\":\"def\"}" `shouldParseAs`
      AwsSetSecurityConfigRequest (MfaSerial "abc") (RoleArn "def")

  describe "AwsSetMFACodeRequest" $ do
    "{\"mfa_code\":\"123456\"}" `shouldParseAs` AwsSetMFACodeRequest (MfaCode "123456")
