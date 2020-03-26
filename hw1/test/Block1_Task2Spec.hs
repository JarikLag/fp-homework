module Block1_Task2Spec 
  ( fullSpec
  ) where

import Test.Hspec (SpecWith, errorCall, describe, it, shouldBe, shouldThrow)
import Control.Exception (evaluate)

import Block1_Task2

fullSpec :: SpecWith ()
fullSpec =
  do
    fromNatSpec
    toNatSpec
    addSpec
    mulSpec
    subSpec
    divNatSpec
    modNatSpec
    isEqualSpec
    isEvenSpec
    compareNatsSpec

fromNatSpec :: SpecWith ()
fromNatSpec =
  describe "Block1_Task2.fromNat" $ do
    it "fromNat Z == 0" $ do
      (fromNat Z) `shouldBe` 0
    it "fromNat S(S(S Z)) == 3" $ do
      (fromNat (S(S(S Z)))) `shouldBe` 3

toNatSpec :: SpecWith ()
toNatSpec =
  describe "Block1_Task2.toNat" $ do
    it "toNat 0 == Z" $ do
      (toNat 0) `shouldBe` Z
    it "toNat 5 == S(S(S(S(S Z))))" $ do
      (toNat 5) `shouldBe` (S(S(S(S(S Z)))))
    it "toNat from negative number is forbidden" $ do
       evaluate (toNat (-1)) `shouldThrow` errorCall "Natural number cannot be less than zero"

addSpec :: SpecWith ()
addSpec =
  describe "Block1_Task2.add" $ do
    it "2 + 0 == 2" $ do
      (add (S(S Z)) Z) `shouldBe` (S(S Z))
    it "3 + 2 == 5" $ do
      (add (S(S(S Z))) (S(S Z))) `shouldBe` (S(S(S(S(S Z)))))

mulSpec :: SpecWith ()
mulSpec =
  describe "Block1_Task2.mul" $ do
    it "2 * 0 == 0" $ do
      (mul (S(S Z)) Z) `shouldBe` (Z)
    it "4 * 2 == 8" $ do
      (mul (S(S(S(S Z)))) (S(S Z))) `shouldBe` (S(S(S(S(S(S(S(S Z))))))))

subSpec :: SpecWith ()
subSpec =
  describe "Block1_Task2.sub" $ do
    it "2 - 0 == 2" $ do
      (sub (S(S Z)) Z) `shouldBe` (S(S Z))
    it "3 - 2 == 1" $ do
      (sub (S(S(S Z))) (S(S Z))) `shouldBe` (S Z)
    it "1 - 4 == 0" $ do
      (sub (S Z) (S(S(S(S Z))))) `shouldBe` (Z)

divNatSpec :: SpecWith ()
divNatSpec =
  describe "Block1_Task2.divNat" $ do
    it "Division by zero is forbidden" $ do
      evaluate (divNat (S(S Z)) Z) `shouldThrow` errorCall "Division by zero"
    it "3 / 2 == 1" $ do
      (divNat (S(S(S Z))) (S(S Z))) `shouldBe` (S Z)
    it "0 / 4 == 0" $ do
      (divNat (Z) (S(S(S(S Z))))) `shouldBe` (Z)
    it "4 / 2 == 2" $ do
      (divNat (S(S(S(S Z)))) (S(S Z))) `shouldBe` (S(S Z))

modNatSpec :: SpecWith ()
modNatSpec =
  describe "Block1_Task2.modNat" $ do
    it "Modulo by zero is forbidden" $ do
      evaluate (modNat (S(S Z)) Z) `shouldThrow` errorCall "Modulo by zero"
    it "3 mod 2 == 1" $ do
      (modNat (S(S(S Z))) (S(S Z))) `shouldBe` (S Z)
    it "3 mod 4 == 3" $ do
      (modNat (S(S(S Z))) (S(S(S(S Z))))) `shouldBe` (S(S(S Z)))
    it "4 mod 2 == 0" $ do
      (modNat (S(S(S(S Z)))) (S(S Z))) `shouldBe` (Z) 

isEqualSpec :: SpecWith ()
isEqualSpec =
  describe "Block1_Task2.isEqual" $ do
    it "2 == 2" $ do
      (isEqual (S(S Z)) (S(S Z))) `shouldBe` True
    it "3 != 1" $ do
      (isEqual (S(S(S Z))) (S Z)) `shouldBe` False

isEvenSpec :: SpecWith ()
isEvenSpec =
  describe "Block1_Task2.isEven" $ do
    it "2 is even" $ do
      (isEven (S(S Z))) `shouldBe` True
    it "5 is not even" $ do
      (isEven (S(S(S(S(S Z)))))) `shouldBe` False

compareNatsSpec :: SpecWith ()
compareNatsSpec =
  describe "Block1_Task2.compareNats" $ do
    it "1 < 3" $ do
      (compareNats (S Z) (S(S(S Z)))) `shouldBe` LT
    it "5 >= 2" $ do
      (compareNats (S(S(S(S(S Z))))) (S(S Z))) `shouldBe` GT