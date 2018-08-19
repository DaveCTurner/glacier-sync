{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Upload where

import           Control.Concurrent                          (forkIO,
                                                              myThreadId,
                                                              threadDelay)
import           Control.Lens                                (runIdentity, (&),
                                                              (.~))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Crypto.Hash                                 (Digest, SHA256)
import           Crypto.Hash                                 (HashAlgorithm,
                                                              hashFinalize,
                                                              hashInit,
                                                              hashUpdate)
import qualified Data.ByteString                             as B
import qualified Data.ByteString.Base64                      as B64
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List                           as DCL
import           Data.IORef
import           Data.Monoid
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as T
import           Data.Time
import           Data.Time.ISO8601
import           Foreign.C.Types
import           GHC.Conc.Sync                               (labelThread)
import           Network.AWS                                 (Env)
import           Network.AWS.Data.Body
import           Network.AWS.Glacier.CompleteMultipartUpload
import           Network.AWS.Glacier.InitiateMultipartUpload
import           Network.AWS.Glacier.UploadMultipartPart
import           System.Posix.Files

import           API.Types
import           Context
import           Treehash

-- TODO introduce "task" concept allowing for progress reporting, cancellation

partSize :: Int
partSize = 16 * 1024 * 1024

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
  let CTime mt = modificationTime fileStatus
      imu = initiateMultipartUpload "-" (startUploadRequestVaultName rq)
              & imuArchiveDescription .~ Just ("<m><v>2</v><p>"
                          <> T.decodeUtf8 (B64.encode $ T.encodeUtf8 $ startUploadRequestPath rq)
                          <> "</p><lm>"
                          <> T.filter (\c -> ('0' <= c && c <= '9') || c == 'T')
                                  (T.takeWhile (/= '.') $ T.pack $ formatISO8601Millis
                                      $ addUTCTime (fromIntegral mt) $ UTCTime (fromGregorian 1970 1 1) 0)
                          <> "Z</lm></m>")
              & imuPartSize           .~ Just (T.pack $ show partSize)

  print imu

  let uploadId = "TODO"

  rateLimiter <- newRateLimiter 50000 50000

  (totalSize, finalChecksum) <- runResourceT $ runConduit $ sourceFile filename
    .| splitIntoParts
    .| uploadPart (startUploadRequestVaultName rq) uploadId rateLimiter
    .| fuseBoth sumSizes combineBlocks
  print (totalSize, finalChecksum)

  do
    (rereadSize, rereadChecksum) <- runResourceT $ runConduit $ sourceFile filename
      .| DCL.map (\bs -> (B.length bs, bs))
      .| fuseBoth sumSizes treehash
    print (rereadSize, rereadChecksum)

  let cmu = completeMultipartUpload "-" (startUploadRequestVaultName rq) uploadId
              & cmuChecksum    .~ Just (T.pack $ show finalChecksum)
              & cmuArchiveSize .~ Just (T.pack $ show totalSize)

  print cmu

  return ()

splitIntoParts :: Monad m => ConduitM B.ByteString (Integer, [B.ByteString]) m ()
splitIntoParts = go 0 0 []
  where
  go currentSize chunkStartPosition currentChunks = await >>= \case
    Nothing -> when (currentSize > 0)
             $ yield (chunkStartPosition, reverse currentChunks)
    Just chunk
      | B.null chunk -> go currentSize chunkStartPosition currentChunks
      | otherwise    -> let newSize = currentSize + B.length chunk
                        in if newSize < partSize
                            then go newSize chunkStartPosition (chunk : currentChunks)
                            else let (chunkInThisPart, chunkInNextPart) = B.splitAt (partSize - currentSize) chunk
                              in do
                                yield (chunkStartPosition, reverse $ chunkInThisPart : currentChunks)
                                leftover chunkInNextPart
                                go 0 (chunkStartPosition + fromIntegral (currentSize + B.length chunkInThisPart)) []

uploadPart :: MonadIO m => T.Text -> T.Text -> RateLimiter -> ConduitM (Integer, [B.ByteString]) (Int, Digest SHA256) m ()
uploadPart vn uid rateLimiter = awaitForever $ \(partStartPosition, partChunks) -> let
  len = sum (map B.length partChunks)
  trh = runIdentity $ runConduit $ DCL.sourceList partChunks .| treehash
  hsh = runIdentity $ runConduit $ DCL.sourceList partChunks .| normalHash
  yieldsChunks = DCL.sourceList partChunks .| rateLimitedConduit rateLimiter
  ump = uploadMultipartPart "-" vn uid (HashedStream hsh (fromIntegral len) yieldsChunks)
    & umpChecksum .~ Just (T.pack $ show trh)
    & umpRange    .~ Just (T.pack $ "bytes " ++ show partStartPosition ++ "-" ++ show (partStartPosition + fromIntegral len) ++ "/*")
  in do
    liftIO $ print ump
    liftIO $ runResourceT $ runConduit $ yieldsChunks .| awaitForever (\chunk ->
      liftIO $ print $ B.length chunk
      )
    yield (len, trh)

sumSizes :: Monad m => ConduitM (Int, a) a m Integer
sumSizes = go 0
  where
  go runningTotal = await >>= \case
    Nothing -> return runningTotal
    Just (len, passthru) -> do
      yield passthru
      go $! runningTotal + fromIntegral len

normalHash :: (Monad m, HashAlgorithm d) => ConduitM B.ByteString Void m (Digest d)
normalHash = go hashInit
  where
  go ctx = await >>= \case
    Nothing -> return $ hashFinalize ctx
    Just bs -> go $ hashUpdate ctx bs

rateLimitedConduit :: MonadIO m => RateLimiter -> ConduitM B.ByteString B.ByteString m ()
rateLimitedConduit rateLimiter = awaitForever uploadChunk
  where
  uploadChunk chunk = unless (B.null chunk) $ do
    maxPermitted <- liftIO $ getPermitted rateLimiter $ B.length chunk
    let (okToSend, notToSend) = B.splitAt maxPermitted chunk
    unless (B.null okToSend) $ yield okToSend
    unless (B.null notToSend) $ do
      liftIO $ threadDelay 250000
      leftover notToSend

data RateLimiter = RateLimiter
  { lastSampleVar  :: IORef (UTCTime, Int)
  , bucketCapacity :: Int
  , leakRate       :: Double
  }

newRateLimiter :: Int -> Double -> IO RateLimiter
newRateLimiter cap rate = do
  now <- getCurrentTime
  r <- newIORef (now, 0)
  return $ RateLimiter r cap rate

getPermitted :: RateLimiter -> Int -> IO Int
getPermitted rl desiredUnits = do
  now <- getCurrentTime
  atomicModifyIORef' (lastSampleVar rl) $ \(lastSampleTime, lastSampleLevel) -> let
    elapsedSeconds = fromRational $ toRational $ diffUTCTime now lastSampleTime
    leakedUnits    = floor $ leakRate rl * elapsedSeconds
    levelAfterLeak = max 0 $ lastSampleLevel - leakedUnits
    availableUnits = bucketCapacity rl - levelAfterLeak
    allocatedUnits = min availableUnits desiredUnits
    in if allocatedUnits <= 0
        then ((lastSampleTime, lastSampleLevel), 0)
        else ((now, levelAfterLeak + allocatedUnits), allocatedUnits)
