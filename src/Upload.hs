{-# LANGUAGE OverloadedStrings #-}

module Upload where

import Control.Monad
import Control.Concurrent
import GHC.Conc.Sync (labelThread)
import           Network.AWS
import System.Posix.Files
import System.Posix.Types
import Foreign.C.Types
import Data.Time
import qualified Data.Text as T
import Data.Monoid

import Context
import API.Types

-- TODO introduce "task" concept allowing for progress reporting, cancellation

uploadBackground :: Context -> StartUploadRequest -> IO ()
uploadBackground context rq = void $ forkIO $ do
  t <- myThreadId
  labelThread t $ "upload[" ++ show rq ++ "]"
  putStrLn $ "Uploading " ++ show rq ++ " with " ++ show t
  --awsEnv <- getAwsEnv context
  let awsEnv = undefined context
  upload awsEnv rq

upload :: Env -> StartUploadRequest -> IO ()
upload _env rq = do
  let filename = T.unpack $         startUploadRequestMirrorPath rq
                          <> "/" <> startUploadRequestVaultName  rq
                          <> "/" <> startUploadRequestPath       rq

  fileStatus <- getFileStatus filename
  let COff  sz = fileSize fileStatus
      CTime mt = modificationTime fileStatus
      modTime  = addUTCTime (fromIntegral mt) (UTCTime (fromGregorian 1970 1 1) 0)
  print (sz, modTime)
  return ()
