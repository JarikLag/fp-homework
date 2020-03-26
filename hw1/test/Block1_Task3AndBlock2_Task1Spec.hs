module Block1_Task3AndBlock2_Task1Spec 
  ( fullSpec
  ) where

import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (property)
import Data.List (sort)

import Block1_Task3AndBlock2_Task1

fullSpec :: SpecWith ()
fullSpec =
  do
    isTreeEmptySpec
    getTreeSizeSpec
    findElementSpec
    insertElementSpec
    deleteElementSpec
    fromListSpec
    treeFoldableSpec

isTreeEmptySpec :: SpecWith ()
isTreeEmptySpec =
  describe "Block1_Task3AndBlock2_Task1Spec.isTreeEmpty" $ do
    it "Leaf is empty tree" $ do
      (isTreeEmpty (Leaf :: Tree Int)) `shouldBe` True
    it "Branch is not empty tree" $ do
      (isTreeEmpty 
        ((Branch [1] (Branch [0] Leaf Leaf) Leaf) :: Tree Int)) 
      `shouldBe` False

getTreeSizeSpec :: SpecWith ()
getTreeSizeSpec =
  describe "Block1_Task3AndBlock2_Task1Spec.getTreeSize" $ do
    it "Leaf size is 0" $ do
      (getTreeSize (Leaf :: Tree Int)) `shouldBe` 0
    it "Random tree size" $ do
      (getTreeSize 
        ((Branch [4,4] 
          (Branch [0,0,0] Leaf (Branch [1,1] Leaf Leaf)) Leaf) :: Tree Int)) 
      `shouldBe` 7

findElementSpec :: SpecWith ()
findElementSpec =
  describe "Block1_Task3AndBlock2_Task1Spec.findElement" $ do
    it "No elements is empty Tree" $ do
      (findElement (Leaf :: Tree Int) 0) `shouldBe` Nothing
    it "No element in random tree" $ do
      (findElement ((Branch [4,4] 
        (Branch [0,0,0] Leaf (Branch [1,1] Leaf Leaf)) Leaf) :: Tree Int) 2)
      `shouldBe` Nothing
    it "Element is present in random tree" $ do
      (findElement ((Branch [4,4] 
        (Branch [0,0,0] Leaf (Branch [1,1] Leaf Leaf)) Leaf) :: Tree Int) 1)
      `shouldBe` (Just 1)

insertElementSpec :: SpecWith ()
insertElementSpec =
  describe "Block1_Task3AndBlock2_Task1Spec.insertElement" $ do
    it "Element added to emptyTree" $ do
      (insertElement emptyTree (1 :: Integer)) `shouldBe` (Branch [1] Leaf Leaf)
    it "New element added to random tree" $ do
      (insertElement (Branch [1] Leaf Leaf) (0 :: Integer))
      `shouldBe` (Branch [1] (Branch [0] Leaf Leaf) Leaf)
    it "Existing element added to random tree" $ do
      (insertElement (Branch [1] Leaf Leaf) (1 :: Integer))
      `shouldBe` (Branch [1,1] Leaf Leaf)

deleteElementSpec :: SpecWith ()
deleteElementSpec =
  describe "Block1_Task3AndBlock2_Task1Spec.deleteElement" $ do
    it "Nothing to delete from empty tree" $ do
      (deleteElement emptyTree (1 :: Int)) `shouldBe` (Leaf :: Tree Int)
    it "Delete element without child" $ do
      (deleteElement (Branch [1] (Branch [0] Leaf Leaf) Leaf) (0 :: Int)) 
      `shouldBe` (Branch [1] Leaf Leaf)
    it "Delete element, but there were more than 1 in node" $ do
      (deleteElement (Branch [1] (Branch [0, 0] Leaf Leaf) Leaf) (0 :: Int)) 
      `shouldBe` (Branch [1] (Branch [0] Leaf Leaf) Leaf)
    it "Delete element with childs" $ do
      (deleteElement 
        (Branch [5] (Branch [4] (Branch [3] Leaf Leaf) Leaf) (Branch [6] Leaf Leaf))
        (5 :: Int))
      `shouldBe` (Branch [6] (Branch [4] (Branch [3] Leaf Leaf) Leaf) Leaf)
    it "Delete element with childs while childs have more childs" $ do
      (deleteElement 
        (Branch [5] 
          (Branch [4] (Branch [3] Leaf Leaf) Leaf) 
          (Branch [10] (Branch [9] Leaf Leaf) (Branch [11] Leaf Leaf)))
        (5 :: Int))
      `shouldBe` (Branch [9] 
        (Branch [4] (Branch [3] Leaf Leaf) Leaf) 
        (Branch [10] Leaf (Branch [11] Leaf Leaf)))

fromListSpec :: SpecWith ()
fromListSpec =
  describe "Block1_Task3AndBlock2_Task1Spec.fromList" $ do
    it "Empty tree from empty list" $ do
      (fromList []) `shouldBe` (Leaf :: Tree Int)
    it "Tree from random list" $ do
      (fromList ([4,1,2,10,7,4] :: [Int])) `shouldBe`
        Branch [4,4] 
        (Branch [1] Leaf (Branch [2] Leaf Leaf)) 
        (Branch [10] (Branch [7] Leaf Leaf) Leaf)

treeFoldableSpec :: SpecWith ()
treeFoldableSpec = 
  describe "Block1_Task3AndBlock2_Task1Spec.TreeFoldableInstance" $ do
    it "Property toList . fromList == sort is always true" $ do
      property checkEveryList
    where
      checkEveryList :: [Integer] -> Bool
      checkEveryList xs = (toList . fromList) xs == sort xs