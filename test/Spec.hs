{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified API.TypesSpec
import           Test.Hspec
import qualified TreehashSpec

main :: IO ()
main = hspec $ do
  TreehashSpec.spec
  API.TypesSpec.spec
