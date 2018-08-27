{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Exception
import           Control.Monad
import           Database.SQLite.Simple
import           System.Directory

data SchemaVersion
  = SchemaVersion0
  | SchemaVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

intFromSchemaVersion :: SchemaVersion -> Int
intFromSchemaVersion SchemaVersion0 = 0
intFromSchemaVersion SchemaVersion1 = 1

schemaVersionFromInt :: Int -> Maybe SchemaVersion
schemaVersionFromInt = flip lookup [(intFromSchemaVersion schemaVersion, schemaVersion) | schemaVersion <- [minBound..maxBound]]

data BadVersionException = BadVersionException [Int] deriving (Show)
instance Exception BadVersionException

data SchemaUpgradeException = SchemaUpgradeException deriving (Show)
instance Exception SchemaUpgradeException

ensureDatabase :: FilePath -> IO ()
ensureDatabase databasePath = do
  dbExists <- doesFileExist databasePath
  withConnection databasePath $ \conn -> withTransaction conn $ do
    unless dbExists $ do
      execute_ conn "CREATE TABLE schema_version (schema_version INTEGER NOT NULL)"
      execute_ conn "INSERT INTO schema_version (schema_version) VALUES (0)"

    let getVersion :: IO SchemaVersion
        getVersion = do
          onlyVersionInts <- query_ conn "SELECT schema_version FROM schema_version LIMIT 2"
          let versionInts    = [vi | Only vi <- onlyVersionInts]
          let schemaVersions = [sv | vi <- versionInts, Just sv <- [schemaVersionFromInt vi]]
          case (onlyVersionInts, schemaVersions) of
            ([_], [sv]) -> return sv
            _           -> throwIO $ BadVersionException versionInts

        upgradeTo target = do
          current <- getVersion
          when (current < target) $ do
            doUpgradeTo target
            execute conn "UPDATE schema_version SET schema_version = ?" $ Only $ intFromSchemaVersion target

        doUpgradeTo SchemaVersion0 = throwIO SchemaUpgradeException

        doUpgradeTo SchemaVersion1 = do
          upgradeTo SchemaVersion0
          execute_ conn $ mconcat
            [ "CREATE TABLE local_inventory"
            , "( vault          TEXT    NOT NULL"
            , ", path           TEXT    NOT NULL"
            , ", size           INTEGER NOT NULL"
            , ", modified_at    TEXT    NOT NULL"
            , ", treehash       TEXT    NOT NULL"
            , ", inventoried_at TEXT    NOT NULL"
            , ", PRIMARY KEY (vault, path)"
            , ") "
            ]

    upgradeTo maxBound
