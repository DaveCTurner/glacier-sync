{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LocalInventory where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Crypto.Hash                      (Digest, HashAlgorithm,
                                                   SHA256, digestFromByteString)
import           Data.Aeson
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Base16           as B16
import           Data.Conduit
import           Data.Conduit.Combinators         (sourceFile)
import qualified Data.HashMap.Strict              as HM
import           Data.Int
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Data.Time
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToField
import           Foreign.C.Types
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Posix.Types

import           CliConfig
import           Context
import           Task
import           Treehash

newtype SerializableDigest a = SerializableDigest (Digest a) deriving (Show, Eq)

instance HashAlgorithm a => ToField (SerializableDigest a)
  where toField (SerializableDigest d) = SQLText $ T.pack $ show d

instance ToJSON (SerializableDigest a)
  where toJSON (SerializableDigest d) = toJSON (show d)

data UnreadableHash = UnreadableHash T.Text deriving (Show)
instance Exception UnreadableHash

instance HashAlgorithm a => FromField (SerializableDigest a)
  where
  fromField f = case fromField f of
    Ok     t      -> maybe (Errors [toException $ UnreadableHash t]) (Ok . SerializableDigest) $ do
      let (bytes, trailing) = B16.decode $ T.encodeUtf8 t
      guard $ B.null trailing
      digestFromByteString bytes
    Errors errors -> Errors errors

data LocalInventoryItem = LocalInventoryItem
  { localInventoryItemVault         :: T.Text
  , localInventoryItemPath          :: T.Text
  , localInventoryItemSize          :: Int64
  , localInventoryItemModifiedAt    :: UTCTime
  , localInventoryItemTreeHash      :: SerializableDigest SHA256
  , localInventoryItemInventoriedAt :: UTCTime
  } deriving (Show, Eq)

instance FromRow LocalInventoryItem where
  fromRow = LocalInventoryItem <$> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON LocalInventoryItem where
  toJSON LocalInventoryItem{..} = object
    [ "vault"         .= localInventoryItemVault
    , "path"          .= localInventoryItemPath
    , "size"          .= localInventoryItemSize
    , "modifiedAt"    .= localInventoryItemModifiedAt
    , "treeHash"      .= localInventoryItemTreeHash
    , "inventoriedAt" .= localInventoryItemInventoriedAt
    ]

instance ToRow LocalInventoryItem where
  toRow LocalInventoryItem{..} = toRow
    ( localInventoryItemVault
    , localInventoryItemPath
    , localInventoryItemSize
    , localInventoryItemModifiedAt
    , localInventoryItemTreeHash
    , localInventoryItemInventoriedAt
    )

data LocalInventory = LocalInventory [LocalInventoryItem]
  deriving (Show, Eq)

instance ToJSON LocalInventory where
  toJSON (LocalInventory items) = object ["items" .= items]

getLocalInventory :: TaskInner -> (LocalInventory -> IO ()) -> IO ()
getLocalInventory taskInner consumeResult = withTransaction (taskDatabaseConnection taskInner) $ do
  consumeResult . LocalInventory
    =<< query_ (taskDatabaseConnection taskInner)
          "SELECT * FROM local_inventory ORDER BY vault, path"

refreshLocalInventory :: Context -> TaskInner -> IO ()
refreshLocalInventory context taskInner
  = withLocalInventorySlot context taskInner
  $ withTransaction (taskDatabaseConnection taskInner)
  $ scanLocalInventory taskInner context

utcTimeFromCTime :: CTime -> UTCTime
utcTimeFromCTime (CTime sec) = addUTCTime (fromIntegral sec) (UTCTime (fromGregorian 1970 1 1) 0)

rebuildLocalInventory :: Context -> TaskInner -> IO ()
rebuildLocalInventory context taskInner
  = withLocalInventorySlot context taskInner
  $ withTransaction (taskDatabaseConnection taskInner) $ do
      execute_ (taskDatabaseConnection taskInner) "DELETE FROM local_inventory"
      scanLocalInventory taskInner context

scanLocalInventory :: TaskInner -> Context -> IO ()
scanLocalInventory taskInner context = do
  -- TODO also need to delete inventory entries for files that no longer exist on disk

  currentEntries <- query_ (taskDatabaseConnection taskInner) "SELECT * FROM local_inventory"
  let currentEntriesByVaultAndPath = HM.fromList [((localInventoryItemVault i, localInventoryItemPath i), i) | i <- currentEntries]

      mirrorRoot = cliConfigDataPath (ctxCliConfig context) </> "mirror"

      onEntry vaultName relPath = do
        atomically (taskSetStatus taskInner $ object
          [ "vault"  .= vaultName
          , "path"   .= relPath
          , "action" .= String "processing entry"
          ]) >>= \case
          Just TaskCancelled -> return ()
          Nothing -> do
            let absPath = mirrorRoot </> vaultName </> relPath
            isFile <- doesFileExist      absPath
            isDir  <- doesDirectoryExist absPath
            when isFile $ onFile vaultName relPath
            when isDir  $ mapM_ (onEntry vaultName . (relPath </>)) =<< listDirectory absPath

      onFile vaultName relPath = do
        let absPath = mirrorRoot </> vaultName </> relPath
        fileStatus <- getFileStatus absPath
        let currentSize  = case fileSize fileStatus of COff  sz  -> sz
            currentMTime = utcTimeFromCTime $ modificationTime fileStatus

        let needsUpdate = case HM.lookup (T.pack vaultName, T.pack relPath) currentEntriesByVaultAndPath of
              Just LocalInventoryItem{..} | localInventoryItemSize == currentSize, localInventoryItemModifiedAt == currentMTime -> False
              _ -> True

        when needsUpdate $ do

          now <- getCurrentTime

          let reportHashProgressConduit !c = await >>= \case
                Nothing -> return ()
                Just bs -> do
                  let c' = c + B.length bs
                  liftIO (atomically $ taskSetStatus taskInner $ object
                    [ "vault"     .= vaultName
                    , "path"      .= relPath
                    , "action"    .= String "calculating hash"
                    , "processed" .= c'
                    , "total"     .= currentSize
                    ]) >>= \case
                      Just TaskCancelled -> return ()
                      Nothing            -> yield bs >> reportHashProgressConduit c'
          treeHashValue <- runResourceT $ runConduit $ sourceFile absPath .| reportHashProgressConduit 0 .| treehash

          atomically (taskSetStatus taskInner $ object
            [ "vault"  .= vaultName
            , "path"   .= relPath
            , "action" .= String "updating inventory"
            ]) >>= \case
            Just TaskCancelled -> return ()
            Nothing -> do

              execute (taskDatabaseConnection taskInner)
                "DELETE FROM local_inventory WHERE vault = ? AND path = ?" (vaultName, relPath)

              execute (taskDatabaseConnection taskInner)
                "INSERT INTO local_inventory VALUES (?,?,?,?,?,?)" LocalInventoryItem
                  { localInventoryItemVault         = T.pack vaultName
                  , localInventoryItemPath          = T.pack relPath
                  , localInventoryItemSize          = case fileSize fileStatus of COff  sz  -> sz
                  , localInventoryItemModifiedAt    = utcTimeFromCTime $ modificationTime fileStatus
                  , localInventoryItemTreeHash      = SerializableDigest treeHashValue
                  , localInventoryItemInventoriedAt = now
                  }

  vaultNames <- listDirectory mirrorRoot
  forM_ vaultNames $ \vaultName -> mapM_ (onEntry vaultName) =<< listDirectory (mirrorRoot </> vaultName)



