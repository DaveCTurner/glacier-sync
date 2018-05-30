module Treehashes where

import           Context
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Crypto.Hash
import qualified Data.Attoparsec.ByteString        as AP
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Base16            as B16
import           Data.Conduit
import           Data.Conduit.Attoparsec           (sinkParser)
import           Data.Conduit.Combinators          (sourceDirectoryDeep,
                                                    sourceFile)
import qualified Data.Conduit.List                 as DCL
import           Data.Word
import           Data.Word8
import           System.FilePath

data TreehashesEntry = TreehashesEntry
  { treehashesEntryFilename :: B.ByteString
  , treehashesEntryDigest   :: Digest SHA256
  } deriving (Show, Eq)

treeHashesParser :: AP.Parser [TreehashesEntry]
treeHashesParser = many treeHashLineParser <* AP.endOfInput

treeHashLineParser :: AP.Parser TreehashesEntry
treeHashLineParser = flip TreehashesEntry
  <$> treeHashDigestParser
  <*  AP.word8 0x20
  <*> treeHashFilenameParser
  <*  AP.word8 0x0a

isBase16Char :: Word8 -> Bool
isBase16Char c = (_0 <= c && c <= _9) || (_a <= c && c <= _f)

treeHashDigestParser :: AP.Parser (Digest SHA256)
treeHashDigestParser = do
  (bytes, trailing) <- B16.decode . B.pack <$> replicateM 64 (AP.satisfy isBase16Char)
  unless (B.null trailing) $ fail "invalid base 16"
  maybe (fail "invalid hash") return $ digestFromByteString bytes

treeHashFilenameParser :: AP.Parser B.ByteString
treeHashFilenameParser = AP.takeWhile (/= _lf)

data TreehashesCollection = TreehashesCollection
  { treehashesCollectionName    :: FilePath
  , treehashesCollectionEntries :: [TreehashesEntry]
  } deriving (Show, Eq)

loadTreehashes :: Context.Context -> IO [TreehashesCollection]
loadTreehashes context = runResourceT $ runConduit
  $  sourceDirectoryDeep False (configPath context </> "treehashes")
  .| awaitForever (\p -> do
        entries <- sourceFile p .| sinkParser treeHashesParser
        yield $ TreehashesCollection p entries)
  .| DCL.consume
