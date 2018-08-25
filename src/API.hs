{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           API.Types
import Task

import           Servant.API

type API =    "security" :> SecurityAPI
         :<|> "upload"   :> UploadAPI
         :<|> "tasks"    :> TasksAPI

type SecurityAPI = "aws" :> SecurityAwsAPI

type SecurityAwsAPI =    "credentials" :> AwsSetCredentialsAPI
                    :<|> "config"      :> AwsSetSecurityConfigAPI
                    :<|> "mfa_code"    :> AwsSetMFACodeAPI
                    :<|> "status"      :> AwsGetStatusAPI

type AwsSetCredentialsAPI    = SetJSON AwsSetCredentialsRequest
type AwsSetSecurityConfigAPI = SetJSON AwsSetSecurityConfigRequest
type AwsSetMFACodeAPI        = SetJSON AwsSetMFACodeRequest
type AwsGetStatusAPI         = Get '[JSON] AwsGetStatusResponse

type SetJSON a = ReqBody '[JSON] a :> PostAccepted '[PlainText] NoContent

type UploadAPI = ReqBody '[JSON] StartUploadRequest :> PostAccepted '[JSON] Task

type TasksAPI = Get '[JSON] [Task]
  :<|> Capture "taskId" Integer :> TaskAPI

type TaskAPI = Get '[JSON] TaskStatus
          :<|> Delete '[PlainText] NoContent
