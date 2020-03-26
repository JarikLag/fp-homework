module Block4_Task1Spec 
  ( fullSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import Block4_Task1

fullSpec :: SpecWith ()
fullSpec = 
  do
    stringSumSpec

stringSumSpec :: SpecWith ()
stringSumSpec =
  describe "Block4_Task1.stringSum" $ do
    it "Calculates sum of ints" $ do
      ((stringSum "4 3 2 10") :: Maybe Int) `shouldBe` (Just 19)
    it "Fails on invalid input" $ do
      ((stringSum "10 20 30 40 fifty") :: Maybe Int) `shouldBe` Nothing
    it "Parses numbers, separated by any types of spaces" $ do
      ((stringSum "3 \n 1 \n\n    \n\t 1 \t\t \n \t\t  5   \n\t   1 ") :: Maybe Int)
      `shouldBe` (Just 11)