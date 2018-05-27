module LibMain where

import           Context
import           Control.Concurrent.Async
import           Lister
import           Network.Wai.Handler.Warp
import           Server

main :: IO ()
main = do
  context <- makeEmptyContext
  withAsync (lister context) $ \_ -> do
    run 8081 $ application context
