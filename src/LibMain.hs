module LibMain where

import           CliConfig
import           Context
import           Control.Concurrent.Async
import           Lister
import           Network.Wai.Handler.Warp
import           Server
import           StoredCredentials
import           Treehashes

main :: IO ()
main = do
  context <- makeEmptyContext =<< parseCliConfig
  loadStoredCredentialsIfAvailable context
  treeHashesCollections <- loadTreehashes context
  print $ map (length . treehashesCollectionEntries) treeHashesCollections
  withAsync (lister context) $ \_ -> do
    run 8081 $ application context
