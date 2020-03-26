module Block2_Task2Spec 
  ( fullSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import Data.List.NonEmpty (NonEmpty (..))

import Block2_Task2

fullSpec :: SpecWith ()
fullSpec = 
  do
    splitOnSpec
    joinWithSpec

splitOnSpec :: SpecWith ()
splitOnSpec = 
  describe "Block2_Task2.splitOn" $ do
    it "Splits string by /" $ do
      (splitOn '/' "path/to/file") `shouldBe` ("path" :| ["to", "file"])
    it "Splits int list by 10" $ do
      (splitOn (10 :: Int) [10, 1, 2, 10, 2, 4, 10]) `shouldBe` 
        ([] :| [[1, 2], [2, 4], []])

joinWithSpec :: SpecWith ()
joinWithSpec =
  describe "Block2_Task2.joinWith" $ do
    it "Joins the string by /" $ do 
      (joinWith '/' ("path" :| ["to", "file"])) `shouldBe` "path/to/file"
    it "Joins the int list by 5" $ do
      (joinWith (5 :: Int) ([] :| [[1, 2], [2, 4], []])) `shouldBe`
        ([5, 1, 2, 5, 2, 4, 5])