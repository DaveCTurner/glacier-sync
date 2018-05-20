{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import qualified TreehashSpec

main :: IO ()
main = hspec $ do
  TreehashSpec.spec

