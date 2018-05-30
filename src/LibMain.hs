module LibMain where

import           Context
import           Control.Concurrent.Async
import           Lister
import           Network.Wai.Handler.Warp
import           Server
import           StoredCredentials
import           Treehashes

main :: IO ()
main = do
  context <- makeEmptyContext
  loadStoredCredentialsIfAvailable context
  treeHashesCollections <- loadTreehashes context
  print treeHashesCollections
  withAsync (lister context) $ \_ -> do
    run 8081 $ application context
