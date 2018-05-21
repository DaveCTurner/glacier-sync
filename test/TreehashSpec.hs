{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module TreehashSpec (spec) where

import Test.Hspec
import Treehash
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Conduit
import qualified Data.Conduit.List as DCL
import Data.Maybe
import Crypto.Hash
import Control.Monad
import Control.DeepSeq

hashFromBase16 :: T.Text -> Either String (Digest SHA256)
hashFromBase16 b16 = maybe (Left $ show b16 ++ " not a valid SHA256 hash") Right
  (digestFromByteString =<< maybeBytes)
  where
  maybeBytes = case B16.decode $ T.encodeUtf8 b16 of
    (bytes, trailing) | B.null trailing -> Just bytes
    _                                   -> Nothing 

combineBlocksIterative :: Monad m => ConduitM (Digest SHA256) Void m (Digest SHA256)
combineBlocksIterative = whileMoreLevels <$> DCL.consume
  where
  whileMoreLevels [] = hashFinalize hashInit
  whileMoreLevels [x] = x
  whileMoreLevels xs = xs `deepseq` whileMoreLevels (nextLevel xs)

  nextLevel [] = []
  nextLevel [x] = [x]
  nextLevel (x1:x2:xs) = concatHashes x1 x2 : nextLevel xs

  concatHashes h1 h2 = hashFinalize $ hashUpdates hashInit [h1,h2]

spec :: Spec
spec = describe "Treehash" $ do
  describe "combineBlocks" $ do
    forM_ [0..100] $ \blockCount ->
      it ("agrees with the naive implementation on " ++ show blockCount ++ " blocks")
        $ let calculateWith f = runConduitPure $ mapM_ yieldHash [1..blockCount] .| f
              yieldHash n = yield $ hashFinalize $ hashUpdate hashInit $ B.replicate n 0x00
        in calculateWith combineBlocks `shouldBe` calculateWith combineBlocksIterative

    let yieldBlock = yield $ hashFinalize hashInit
        yieldBlocks n = if n > 0 then yieldBlock >> yieldBlocks (n-1) else return ()

    it "calculates an unreasonably large number of blocks" $
      Right (runConduitPure $ yieldBlocks 100000 .| combineBlocks)
        `shouldBe` hashFromBase16 "cad3bee1b0141317ae257f7d72d7586bd1301901155dabf5a718f9c0bdfa3d3a"

  describe "blockhash" $ do
    let isHashesOfZeroes :: [T.Text] -> [Int] -> Spec
        isHashesOfZeroes expectedResultsBase16 blockSizes = it ("works on " ++ show blockSizes)
          $ actualResult `shouldBe` expectedResult
          where
            actualResult = map Right $ runConduitPure
              $ mapM_ (\n -> yield (B.replicate n 0x00)) blockSizes .| blockhash .| DCL.consume
            expectedResult = map hashFromBase16 expectedResultsBase16

    describe "empty" $ do
      ["e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"] `isHashesOfZeroes` []
      ["e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"] `isHashesOfZeroes` [0]
      ["e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"] `isHashesOfZeroes` (replicate 10 0)

    describe "1 block" $ do
      ["6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"] `isHashesOfZeroes` [1]
      ["6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"] `isHashesOfZeroes` [1,0]
      ["6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"] `isHashesOfZeroes` [0,1]
      ["96a296d224f285c67bee93c30f8a309157f0daa35dc5b87e410b78630a09cfc7"] `isHashesOfZeroes` [2]
      ["96a296d224f285c67bee93c30f8a309157f0daa35dc5b87e410b78630a09cfc7"] `isHashesOfZeroes` [1,1]
      ["01d448afd928065458cf670b60f5a594d735af0172c8d67f22a81680132681ca"] `isHashesOfZeroes` [10]
      ["cd00e292c5970d3c5e2f0ffa5171e555bc46bfc4faddfb4a418b6840b86e79a3"] `isHashesOfZeroes` [100]
      ["30e14955ebf1352266dc2ff8067e68104607e750abb9d3b36582b8af909fcb58"] `isHashesOfZeroes` [1024*1024]
      ["30e14955ebf1352266dc2ff8067e68104607e750abb9d3b36582b8af909fcb58"] `isHashesOfZeroes` (replicate 128 8192)

    describe "2 blocks" $ do
      [ "30e14955ebf1352266dc2ff8067e68104607e750abb9d3b36582b8af909fcb58"
       ,"6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"] `isHashesOfZeroes` [1024*1024 + 1]
      [ "30e14955ebf1352266dc2ff8067e68104607e750abb9d3b36582b8af909fcb58"
       ,"6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"] `isHashesOfZeroes` [1, 1024*1024]
      [ "30e14955ebf1352266dc2ff8067e68104607e750abb9d3b36582b8af909fcb58"
       ,"30e14955ebf1352266dc2ff8067e68104607e750abb9d3b36582b8af909fcb58"] `isHashesOfZeroes` [1024*1024*2]

    describe "more blocks" $ do
      (replicate 10 "30e14955ebf1352266dc2ff8067e68104607e750abb9d3b36582b8af909fcb58")
        `isHashesOfZeroes` [10*1024*1024]

      (replicate 10 "30e14955ebf1352266dc2ff8067e68104607e750abb9d3b36582b8af909fcb58"
                ++ ["6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"])
        `isHashesOfZeroes` [10*1024*1024 + 1]

      (replicate 10 "30e14955ebf1352266dc2ff8067e68104607e750abb9d3b36582b8af909fcb58"
                ++ ["6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"])
        `isHashesOfZeroes` (1 : replicate 10 (1024*1024))

  describe "treeHash" $ do
    -- test calculating treehashes of sequences of zeroes of various lengths
    let isHashOfZeroes :: T.Text -> [Int] -> Spec
        isHashOfZeroes expectedResultBase16 blockSizes
          = it ("calculates the treehash of " ++ show blockSizes ++ " zeroes")
            $ actualResult `shouldBe` expectedResult
          where
            actualResult = Right $ runConduitPure $ mapM_ (\n -> yield (B.replicate n 0x00)) blockSizes .| treehash
            expectedResult = hashFromBase16 expectedResultBase16

    describe "empty hash" $ do
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" `isHashOfZeroes` []
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" `isHashOfZeroes` [0]
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" `isHashOfZeroes` (replicate 10 0)

    describe "1 block" $ do
      "6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d" `isHashOfZeroes` [1]
      "96a296d224f285c67bee93c30f8a309157f0daa35dc5b87e410b78630a09cfc7" `isHashOfZeroes` [2]
      "01d448afd928065458cf670b60f5a594d735af0172c8d67f22a81680132681ca" `isHashOfZeroes` [10]
      "cd00e292c5970d3c5e2f0ffa5171e555bc46bfc4faddfb4a418b6840b86e79a3" `isHashOfZeroes` [100]
      "30e14955ebf1352266dc2ff8067e68104607e750abb9d3b36582b8af909fcb58" `isHashOfZeroes` [1024*1024]
      "30e14955ebf1352266dc2ff8067e68104607e750abb9d3b36582b8af909fcb58" `isHashOfZeroes` (replicate 128 8192)

    describe "2 blocks" $ do
      "28638dab8d5e1754a4ecb38b0ebe6df66c844f94aed142d4d0283d208bb786cd" `isHashOfZeroes` [1024*1024 + 1]
      "28638dab8d5e1754a4ecb38b0ebe6df66c844f94aed142d4d0283d208bb786cd" `isHashOfZeroes` (1 : replicate 128 8192)

    describe "more blocks" $ do
      "7130ecc84a289d83a9563a06260785e4081c6c660090052bd5e77c3b0060910b" `isHashOfZeroes` [10*1024*1024]
      "be4acb7214e68ebe7958ac2780caf5138674b0c6f36aa263b8d36a7ed3684139" `isHashOfZeroes` [10*1024*1024 + 1]
