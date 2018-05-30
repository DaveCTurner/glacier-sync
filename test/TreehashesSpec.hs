{-# LANGUAGE OverloadedStrings #-}

module TreehashesSpec where

import           Crypto.Hash
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import           Test.Hspec
import           Treehashes

spec :: Spec
spec = describe "TreehashesSpec" $ do
  describe "parsing" $ do
    let shouldParseAs inputLines expectedRaw = it ("parses " ++ show inputLines)
         $ AP.parseOnly treeHashesParser input `shouldBe` expected
          where input = mconcat (map (flip B.snoc 0x0a) inputLines)
                expected = mapM mkExpected expectedRaw
                mkExpected (fileName, rawHash) = TreehashesEntry fileName <$> case B16.decode rawHash of
                    (bytes, trailing) | B.null trailing
                        -> maybe (Left "invalid hash") Right $ digestFromByteString bytes
                    _   -> Left "not base 16"

    [] `shouldParseAs` []

    ["e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 /dev/null"]
      `shouldParseAs`
      [("/dev/null", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
      ]

    [ "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 /dev/null"
     ,"9aad85c0c65d07433f4069d8c7e6ad6d6ad44bde6b7761e813209e08aacf791d otherfile"]
      `shouldParseAs`
      [("/dev/null", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
      ,("otherfile", "9aad85c0c65d07433f4069d8c7e6ad6d6ad44bde6b7761e813209e08aacf791d")
      ]

    it "accepts file with one line"
      $ AP.parseOnly treeHashesParser "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 /dev/null\n"
      `shouldBe` Right [TreehashesEntry "/dev/null" $ hashFinalize hashInit]

    let shouldReject description input = it ("rejects " ++ description)
          $ AP.parseOnly treeHashesParser input `shouldBe` Left "endOfInput"

    shouldReject "file with no trailing newline"          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 /dev/null"
    shouldReject "file with too-short hash"               "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b85 /dev/null\n"
    shouldReject "file with too-long hash"                "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b8550 /dev/null\n"
    shouldReject "file with invalid character in hash"    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b85Z /dev/null\n"
    shouldReject "file with invalid character after hash" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855Z/dev/null\n"
