module Main 
  ( main
  ) where

import Test.Hspec (hspec)

import FileManagerSpec (fullSpec)
import ParserSpec (fullSpec)

main :: IO ()
main = hspec $ do
  FileManagerSpec.fullSpec
  ParserSpec.fullSpec