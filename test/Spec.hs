{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec

import qualified API.TypesSpec
import qualified TreehashesSpec
import qualified TreehashSpec

main :: IO ()
main = hspec $ do
  TreehashSpec.spec
  API.TypesSpec.spec
  TreehashesSpec.spec
