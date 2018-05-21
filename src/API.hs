{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import API.Types

import Servant.API

type API = "security" :> SecurityAPI

type SecurityAPI = "aws" :> SecurityAwsAPI

type SecurityAwsAPI =    "credentials" :> AwsSetCredentialsAPI
                    :<|> "config"      :> AwsSetSecurityConfigAPI
                    :<|> "mfa_code"    :> AwsSetMFACodeAPI

type AwsSetCredentialsAPI    = SetJSON AwsSetCredentialsRequest 
type AwsSetSecurityConfigAPI = SetJSON AwsSetSecurityConfigRequest
type AwsSetMFACodeAPI        = SetJSON AwsSetMFACodeRequest

type SetJSON a = ReqBody '[JSON] a :> PostAccepted '[PlainText] NoContent

