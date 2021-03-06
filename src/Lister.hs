{-# LANGUAGE OverloadedStrings #-}

module Lister where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens                   hiding (Context)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Conduit
import qualified Data.Conduit.List              as DCL
import           Network.AWS
import           Network.AWS.Glacier.ListJobs
import           Network.AWS.Glacier.ListVaults
import           Network.AWS.Glacier.Types

import           Context

showException :: SomeException -> IO ()
showException = print

lister :: Context -> IO ()
lister context = handle showException $ do

  liftIO $ putStrLn "waiting for AWS env"
  (env, _) <- atomically $ maybe retry return . veEnv =<< readTVar (awsEnvVar context)
  liftIO $ putStrLn "got AWS env"

  runResourceT $ runAWS env $ forM_ [Ireland] $ \region -> do
    liftIO $ putStrLn $ "Region: " ++ show region
    within region $ runConduit
      $  paginate (listVaults "-")
      .| DCL.concatMap (^. lvrsVaultList)
      .| awaitForever (\dvo -> do
        liftIO $ putStrLn $ "vault: " ++ show dvo
        maybe (return ()) yield $ dvo ^. dvoVaultName)
      .| awaitForever (\vaultName -> do
          toProducer (paginate $ listJobs "-" vaultName)
              .| DCL.concatMap (^. ljrsJobList)
              .| awaitForever (liftIO . print)
        )

  liftIO $ putStrLn "end of vault list"
