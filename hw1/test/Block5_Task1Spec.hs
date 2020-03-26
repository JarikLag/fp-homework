module Block5_Task1Spec 
  ( fullSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import Block5_Task1

fullSpec :: SpecWith ()
fullSpec = 
  do
    evalSpec

evalSpec :: SpecWith ()
evalSpec =
  describe "Block5_Task1.eval" $ do
    it "Calculates random example #1" $ do
      eval (Add (Mul (Const 5) (Div (Const 2) (Const 1))) 
        (Sub (Pow (Const 2) (Const 3)) (Const 1))) `shouldBe` Right 17
    it "Calculates random example #2" $ do
      eval (Add (Sub (Const 5) (Const 6)) (Mul (Const 2) (Const 3))) `shouldBe` Right 5
    it "Fails with DivisionByZero" $ do
      eval (Div (Const 42) (Add (Const 3) (Const (-3)))) `shouldBe` Left DivisionByZero
    it "Fails with NegativePow" $ do
      eval (Pow (Const 42) (Mul (Const (-1)) (Const 10))) `shouldBe` Left NegativePow