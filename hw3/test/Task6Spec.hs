module Task6Spec 
  ( fullSpec
  ) where

import Task5
  ( FS (..)
  )
import Task6
  ( cd
  , file
  , ls
  )
import Lens.Micro
  ( (^..)
  , (^?)
  )
import Test.Hspec 
  ( SpecWith
  , describe
  , it
  , shouldBe
  )

fullSpec :: SpecWith ()
fullSpec = do
  describe "Task6 test" $ do
    cdSpec
    lsSpec
    fileSpec

cdSpec :: SpecWith ()
cdSpec = do
  describe "cd command tests" $ do
    it "cd works on existent dirs" $ do
      (rootDir ^.. cd "n1") `shouldBe` [n1Dir]
    it "composition of cd works" $ do
      (rootDir ^? cd "n1" . cd "k1") `shouldBe` (Just $ (Dir "k1"[]))
    it "cd deals with non-existent dirs" $ do
      (rootDir ^? cd "n1" . cd "lol") `shouldBe` Nothing

lsSpec :: SpecWith ()
lsSpec = do
  describe "ls command tests" $ do
    it "ls works on root dir" $ do
      (rootDir ^.. ls) `shouldBe` ["n1", "n2", "n3"]
    it "ls works with cd" $ do
      (rootDir ^.. cd "n1" . ls) `shouldBe` ["k1", "k2", "file.txt", "lol.txt"]
    it "ls works on empty dirs" $ do
      (rootDir ^.. cd "n2" . ls) `shouldBe` []

fileSpec :: SpecWith ()
fileSpec = do
  describe "file command tests" $ do
    it "file finds File" $ do
      (n1Dir ^? file "file.txt") `shouldBe` (Just $ "file.txt")
    it "file finds works with cd" $ do
      (rootDir ^? cd "n1" . file "lol.txt") `shouldBe` (Just $ "lol.txt")
    it "file cannot find non-existent File" $ do
      (rootDir ^? cd "n2" . file "heh.txt") `shouldBe` Nothing

emptyDir :: FilePath -> FS
emptyDir nm = Dir nm []

emptyFile :: FilePath -> FS
emptyFile nm = File nm

rootDir :: FS
rootDir = Dir "root" [n1Dir, emptyDir "n2", emptyDir "n3"]

n1Dir :: FS
n1Dir = Dir "n1" [emptyDir "k1", emptyDir "k2", emptyFile "file.txt", emptyFile "lol.txt"]