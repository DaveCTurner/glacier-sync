{-# LANGUAGE LambdaCase #-}

module Treehash (blockhash, combineBlocks, treehash) where

import           Crypto.Hash
import qualified Data.ByteString          as B
import           Data.Conduit
import qualified Data.Conduit.Combinators as DCC
import           Data.List

treehash :: Monad m => ConduitM B.ByteString Void m (Digest SHA256)
treehash = blockhash .| combineBlocks

blockhash :: Monad m => ConduitM B.ByteString (Digest SHA256) m ()
blockhash = go (1024*1024) hashInit
  where
    go remaining context = await >>= \case
      Nothing -> yield $ hashFinalize context
      Just bytes
        | remaining < B.length bytes -> do
            let (thisBlock, nextBlock) = B.splitAt remaining bytes
            yield $ hashFinalize $ hashUpdate context thisBlock
            leftover nextBlock
            blockhash
        | otherwise -> go (remaining - B.length bytes) (hashUpdate context bytes)

data LevelHash = LevelHash { level :: !Int, hashValue :: !(Digest SHA256) }

combineBlocks :: Monad m => ConduitM (Digest SHA256) Void m (Digest SHA256)
combineBlocks = await >>= \case
  Nothing -> return $ hashFinalize hashInit
  Just h0 -> foldl1' revConcatHashes . map hashValue <$> DCC.foldl go [level0 h0]

  where
    go levelHashes h = combineHashes (level0 h) levelHashes

    level0 h = LevelHash { level = 0, hashValue = h }
    revConcatHashes h1 h2 = hashFinalize $ hashUpdates hashInit [h2,h1]

    combineHashes x1 [] = [x1]
    combineHashes x1 xs@(x2:xs') = if level x1 == level x2 then combineHashes combined xs' else x1 : xs
      where
        combined = LevelHash { level = level x1 + 1, hashValue = revConcatHashes (hashValue x1) (hashValue x2) }
