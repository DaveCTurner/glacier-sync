module Server where

import API
import API.Types
import Servant
import Network.Wai
import Data.Proxy
import Control.Monad.IO.Class

application :: Application
application = serve (Proxy :: Proxy API) serveAPI

serveAPI :: Server API
serveAPI = serveSecurityAPI

serveSecurityAPI :: Server SecurityAPI
serveSecurityAPI = serveSecurityAwsAPI

serveSecurityAwsAPI :: Server SecurityAwsAPI
serveSecurityAwsAPI = serveAwsSetCredentialsAPI
                 :<|> serveAwsSetSecurityConfigAPI

serveAwsSetCredentialsAPI :: Server AwsSetCredentialsAPI
serveAwsSetCredentialsAPI req = do
  liftIO $ print req
  return NoContent

serveAwsSetSecurityConfigAPI :: Server AwsSetSecurityConfigAPI
serveAwsSetSecurityConfigAPI req = do
  liftIO $ print req
  return NoContent
