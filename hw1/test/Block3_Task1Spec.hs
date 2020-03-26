module Block3_Task1Spec 
  ( fullSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import Data.Monoid (Sum (..))

import Block3_Task1

fullSpec :: SpecWith ()
fullSpec = 
  do
    maybeConcatSpec
    eitherConcatSpec

maybeConcatSpec :: SpecWith ()
maybeConcatSpec =
  describe "Block3_Task1.maybeConcat" $ do
    it "Random list of Maybe [Int] concat" $ do
      (maybeConcat ([Just [1,2,3], Nothing, Just [4,5]] :: [Maybe [Int]])) `shouldBe`
        [1, 2, 3, 4, 5]

eitherConcatSpec :: SpecWith ()
eitherConcatSpec = 
  describe "Block3_Task1.eitherConcat" $ do
    it "Random list of (Either (Sum Int) [Int]) concat" $ do
      (eitherConcat 
        ([Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]] 
          :: [Either (Sum Int) [Int]])) 
      `shouldBe`
        ((Sum {getSum = 8}, [1, 2, 3, 4, 5]))