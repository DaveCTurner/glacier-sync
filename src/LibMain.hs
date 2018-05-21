module LibMain where

import Server
import Network.Wai.Handler.Warp
import Context
import Control.Concurrent.STM.TVar
import Network.AWS.Auth
import Control.Concurrent.Async
import Lister

main :: IO ()
main = do
  context <- makeEmptyContext
  withAsync (lister context) $ \_ -> do
    run 8081 $ application context
