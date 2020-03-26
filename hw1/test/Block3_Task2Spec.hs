module Block3_Task2Spec 
  ( fullSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe, shouldSatisfy)

import Block3_Task2

fullSpec :: SpecWith ()
fullSpec =
  do
    nonEmptySemigroupSpec
    thisOrThatSemigroupSpec
    nameSemigroupSpec
    endoSemigroupSpec

nonEmptySemigroupRes :: NonEmpty Char -> Bool
nonEmptySemigroupRes ('a' :| "bcdefgh") = True
nonEmptySemigroupRes _                  = False

nonEmptySemigroupSpec :: SpecWith ()
nonEmptySemigroupSpec =
  describe "Block3_Task2.nonEmptySemigroup" $ do
    it "Operation <> on two random NonEmpty Char" $ do
      ((<>) ('a' :| "bcd") ('e' :| "fgh")) `shouldSatisfy` nonEmptySemigroupRes

thisOrThatSemigroupRes1 :: ThisOrThat [Char] [Char] -> Bool
thisOrThatSemigroupRes1 (Both "a" "b") = True
thisOrThatSemigroupRes1 _              = False

thisOrThatSemigroupRes2 :: ThisOrThat [Char] [Int] -> Bool
thisOrThatSemigroupRes2 (Both "ab" [1,2]) = True
thisOrThatSemigroupRes2 _              = False

thisOrThatSemigroupSpec :: SpecWith ()
thisOrThatSemigroupSpec =
    describe "Block3_Task2.thisOrThatSemigroup" $ do
      it "Operation <> on This and That" $ do
        ((<>) (This "a") (That "b")) `shouldSatisfy` thisOrThatSemigroupRes1
      it "Operation <> on Both and Both" $ do
        ((<>) (Both "a" [1]) (Both "b" [2])) `shouldSatisfy` thisOrThatSemigroupRes2

nameSemigroupRes1 :: Name -> Bool
nameSemigroupRes1 (Name "") = True
nameSemigroupRes1 _         = False

nameSemigroupRes2 :: Name -> Bool
nameSemigroupRes2 (Name "root.server") = True
nameSemigroupRes2 _                    = False

nameSemigroupSpec :: SpecWith ()
nameSemigroupSpec = 
  describe "Block3_Task2.nameSemigroup" $ do
    it "Mempty for Name String is empty String" $ do
      mempty `shouldSatisfy` nameSemigroupRes1
    it "Operation <> for random Names" $ do
      ((<>) (Name "root") (Name "server")) `shouldSatisfy` nameSemigroupRes2

endoSemigroupSpec :: SpecWith ()
endoSemigroupSpec =
  describe "Block3_Task2.endoSemigroup" $ do
    it "Operation <> for random Endo" $ do
     (getEndo ((<>) (Endo id) (Endo (\x -> x + 1)))) (55 :: Int) `shouldBe` 56
      