{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import qualified TreehashSpec
import qualified API.TypesSpec

main :: IO ()
main = hspec $ do
  TreehashSpec.spec
  API.TypesSpec.spec
