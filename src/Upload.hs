{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Upload where

import           Control.Concurrent                          (threadDelay)
import           Control.Concurrent.STM
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
import           Data.Aeson
import qualified Data.ByteString                             as B
import qualified Data.ByteString.Base64                      as B64
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List                           as DCL
import System.FilePath
import           Data.IORef
import           Data.Monoid
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as T
import           Data.Time
import           Data.Time.ISO8601
import           Foreign.C.Types
import           Network.AWS.Data.Body
import           Network.AWS.Glacier.AbortMultipartUpload
import           Network.AWS.Glacier.CompleteMultipartUpload
import           Network.AWS.Glacier.InitiateMultipartUpload
import           Network.AWS.Glacier.UploadMultipartPart
import           System.Posix.Files

import           API.Types
import CliConfig
import           Context
import           Task
import           Treehash

partSize :: Int
partSize = 16 * 1024 * 1024

burstSize :: Int
burstSize = 50000

relativePath :: StartUploadRequest -> FilePath
relativePath StartUploadRequest{..} = T.unpack $ startUploadRequestVaultName <> "/" <> startUploadRequestPath

upload :: Context -> StartUploadRequest -> TaskInner -> IO ()
upload context rq taskInner = withUploaderSlot context taskInner $ do

  let filename = cliConfigDataPath (ctxCliConfig context) </> relativePath rq

  void $ atomically $ taskSetStatus taskInner ("getting file details" :: T.Text)

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

  void $ atomically $ taskSetStatus taskInner ("initiating upload" :: T.Text)

  print imu

  void $ atomically $ taskSetStatus taskInner ("initiated upload" :: T.Text)

  let uploadId = "TODO"

  rateLimiter <- newRateLimiter burstSize 50000

  (totalSize, finalChecksum) <- runResourceT $ runConduit $ sourceFile filename
    .| splitIntoParts
    .| enumerateItems
    .| uploadPart (startUploadRequestVaultName rq) uploadId rateLimiter taskInner
    .| fuseBoth sumSizes combineBlocks
  print (totalSize, finalChecksum)

  statusAfterUpload <- atomically $ taskSetStatus taskInner ("uploaded parts" :: T.Text)

  let abortUpload = do
        void $ atomically $ taskSetStatus taskInner ("aborting upload" :: T.Text)
        let amu = abortMultipartUpload "-" (startUploadRequestVaultName rq) uploadId
        print amu
        void $ atomically $ taskSetStatus taskInner ("aborted upload" :: T.Text)

  case statusAfterUpload of
    Nothing -> do
      (rereadSize, rereadChecksum) <- runResourceT $ runConduit $ sourceFile filename
          .| DCL.map (\bs -> (B.length bs, bs))
          .| fuseBoth sumSizes treehash
      print (rereadSize, rereadChecksum)

      if (rereadSize, rereadChecksum) /= (totalSize, finalChecksum)
        then abortUpload
        else do

          void $ atomically $ taskSetStatus taskInner ("completing upload" :: T.Text)

          let cmu = completeMultipartUpload "-" (startUploadRequestVaultName rq) uploadId
                      & cmuChecksum    .~ Just (T.pack $ show finalChecksum)
                      & cmuArchiveSize .~ Just (T.pack $ show totalSize)

          print cmu

          void $ atomically $ taskSetStatus taskInner ("completed upload" :: T.Text)

    Just TaskCancelled -> abortUpload

enumerateItems :: Monad m => ConduitM a (Integer, a) m ()
enumerateItems = go 0
  where
  go i = awaitForever $ \x -> yield (i,x) >> (go $! i+1)

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

ensureSmallerThan :: Monad m => Int -> ConduitM B.ByteString B.ByteString m ()
ensureSmallerThan maxSize = awaitForever $ \chunk ->
  let (firstChunk, overflow) = B.splitAt maxSize chunk in do
    unless (B.null firstChunk) $ yield firstChunk
    unless (B.null overflow) $ leftover overflow

uploadPart :: MonadIO m => T.Text -> T.Text -> RateLimiter -> TaskInner -> ConduitM (Integer, (Integer, [B.ByteString])) (Int, Digest SHA256) m ()
uploadPart vn uid rateLimiter taskInner = awaitForever $ \(partNumber, (partStartPosition, partChunks)) -> let
  len = sum (map B.length partChunks)
  trh = runIdentity $ runConduit $ DCL.sourceList partChunks .| treehash
  hsh = runIdentity $ runConduit $ DCL.sourceList partChunks .| normalHash
  yieldsChunks = DCL.sourceList partChunks .| rateLimitedConduit rateLimiter .| statusReportingConduit taskInner partNumber
  ump = uploadMultipartPart "-" vn uid (HashedStream hsh (fromIntegral len) yieldsChunks)
    & umpChecksum .~ Just (T.pack $ show trh)
    & umpRange    .~ Just (T.pack $ "bytes " ++ show partStartPosition ++ "-" ++ show (partStartPosition + fromIntegral len) ++ "/*")
  in do
    taskStatus <- liftIO $ atomically $ taskSetStatus taskInner $ object
      [ "activity" .= String "uploading"
      , "part"     .= partNumber
      ]
    case taskStatus of
      Nothing -> do
        liftIO $ print ump
        liftIO $ runResourceT $ runConduit $ yieldsChunks .| showProgress
        yield (len, trh)

      Just TaskCancelled -> return ()

statusReportingConduit :: MonadIO m => TaskInner -> Integer -> ConduitM B.ByteString B.ByteString m ()
statusReportingConduit taskInner partNumber = go 0
  where
    go c = do
      next <- await
      case next of
        Nothing -> void $ liftIO $ atomically $ taskSetStatus taskInner $ object
          [ "activity" .= String "uploaded part"
          , "part"     .= partNumber
          ]
        Just bs -> do
          let c' = c + B.length bs
          taskStatus <- liftIO $ atomically $ taskSetStatus taskInner $ object
            [ "activity" .= String "uploading"
            , "part"     .= partNumber
            , "bytes"    .= c'
            ]
          case taskStatus of
            Nothing -> do
              yield bs
              go $! c'

            Just TaskCancelled -> return ()

showProgress :: MonadIO m => ConduitM B.ByteString Void m ()
showProgress = go =<< liftIO getCurrentTime
  where
  go startTime = loop (0 :: Integer)
    where
    loop bytesSoFar = awaitForever $ \chunk -> let bytesSoFar' = bytesSoFar + fromIntegral (B.length chunk) in do
      liftIO $ do
        now <- getCurrentTime
        putStrLn $ show bytesSoFar' ++ " bytes in " ++ show (diffUTCTime now startTime) ++ " is "
          ++ show (fromIntegral bytesSoFar' / fromRational (toRational $ diffUTCTime now startTime) :: Double) ++ "bps"
      loop bytesSoFar'

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
rateLimitedConduit rateLimiter = ensureSmallerThan (bucketCapacity rateLimiter) .| awaitForever uploadChunk
  where
  uploadChunk chunk = do
    liftIO $ waitForCapacity rateLimiter $ B.length chunk
    yield chunk

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

waitForCapacity :: RateLimiter -> Int -> IO ()
waitForCapacity rl requiredUnits = do
  now <- getCurrentTime
  awaitingUnits <- atomicModifyIORef' (lastSampleVar rl) $ \(lastSampleTime, lastSampleLevel) -> let
    elapsedSeconds = fromRational $ toRational $ diffUTCTime now lastSampleTime
    leakedUnits    = floor $ leakRate rl * elapsedSeconds
    levelAfterLeak = max 0 $ lastSampleLevel - leakedUnits
    availableUnits = bucketCapacity rl - levelAfterLeak
    in if availableUnits < requiredUnits
      then ((lastSampleTime, lastSampleLevel), requiredUnits - availableUnits)
      else ((now, levelAfterLeak + requiredUnits), 0)
  when (awaitingUnits > 0) $ do
    threadDelay $ ceiling $ 1.0e6 * fromIntegral awaitingUnits / leakRate rl
    waitForCapacity rl requiredUnits
