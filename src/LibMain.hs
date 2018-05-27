module LibMain where

import           Context
import           Control.Concurrent.Async
import           Lister
import           Network.Wai.Handler.Warp
import           Server
import StoredCredentials

main :: IO ()
main = do
  context <- makeEmptyContext
  loadStoredCredentialsIfAvailable context
  withAsync (lister context) $ \_ -> do
    run 8081 $ application context
