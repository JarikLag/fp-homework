module Block5_Task2Spec 
  ( fullSpec
  ) where

import Test.Hspec (SpecWith, errorCall, describe, it, shouldBe, shouldThrow)
import Control.Exception (evaluate)

import Block5_Task2

fullSpec :: SpecWith ()
fullSpec = 
  do
    movingSpec

movingSpec :: SpecWith ()
movingSpec =
  describe "Block5_Task2.moving" $ do
    it "Perfrom SMA algorithm #1" $ do
      (moving 4 [1, 5, 3, 8, 7, 9, 6]) `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
    it "Perfrom SMA algorithm #2" $ do
      (moving 2 [1, 5, 3, 8, 7, 9, 6]) `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
    it "Fails on incorrect window size" $ do
      evaluate (moving 0 [1, 5, 3, 8, 7, 9, 6]) 
      `shouldThrow` errorCall "Window size cannot be less than zero" 