module Task7Spec 
  ( fullSpec
  ) where

import Task5
  ( FS (..)
  )
import Task7
  ( changeExtension
  , deleteDirIfEmpty
  , getAllFileNames
  )
import Test.Hspec 
  ( SpecWith
  , describe
  , it
  , shouldBe
  )

fullSpec :: SpecWith ()
fullSpec = do
  describe "Task7 test" $ do
    changeExtSpec
    deleteDirSpec
    getAllNamesSpec

changeExtSpec :: SpecWith ()
changeExtSpec = do
  describe "changeExtension tests" $ do
    it "changes extensions non-recursive" $ do
      (changeExtension "hs" rootDir) `shouldBe` rootDirExt
    it "does nothing with empty dirs" $ do
      (changeExtension "hs" (emptyDir "k")) `shouldBe` (emptyDir "k")

deleteDirSpec :: SpecWith ()
deleteDirSpec = do
  describe "deleteDirIfEmpty tests" $ do
    it "deletes empty dir" $ do
      (deleteDirIfEmpty "n2" rootDir) `shouldBe` rootDirDel
    it "doesn't delete non-empty dir" $ do
      (deleteDirIfEmpty "n1" rootDir) `shouldBe` rootDir
    it "does nothing with empty dirs" $ do
      (deleteDirIfEmpty "n100" (emptyDir "k")) `shouldBe` (emptyDir "k")

getAllNamesSpec :: SpecWith ()
getAllNamesSpec = do
  describe "getAllFileNames tests" $ do
    it "get all names of files recursive" $ do
      (getAllFileNames rootDir) `shouldBe` listOfFiles
    it "empty list on dir without files" $ do
      (getAllFileNames rootDirNoFiles) `shouldBe` []

emptyDir :: FilePath -> FS
emptyDir nm = Dir nm []

emptyFile :: FilePath -> FS
emptyFile nm = File nm

rootDir :: FS
rootDir = Dir "root" [n1Dir, emptyDir "n2", emptyFile "file.txt", emptyFile "heh.txt"]

rootDirDel :: FS
rootDirDel = Dir "root" [n1Dir, emptyFile "file.txt", emptyFile "heh.txt"]

rootDirExt :: FS
rootDirExt = Dir "root" [n1Dir, emptyDir "n2", emptyFile "file.hs", emptyFile "heh.hs"]

rootDirNoFiles :: FS
rootDirNoFiles = Dir "root" [n1DirNoFiles, emptyDir "n2"]

n1Dir :: FS
n1Dir = Dir "n1" [emptyDir "k1", emptyDir "k2", emptyFile "lol.txt"]

n1DirNoFiles :: FS
n1DirNoFiles = Dir "n1" [emptyDir "k1", emptyDir "k2"]

listOfFiles :: [FilePath]
listOfFiles = ["file.txt", "heh.txt", "lol.txt"] 