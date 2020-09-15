module FileManagerSpec 
  ( fullSpec
  ) where

import Test.Hspec 
  ( SpecWith

  , describe
  , it
  , shouldBe
  , shouldContain
  )
import System.Directory 
  ( Permissions (..)

  , emptyPermissions
  , setOwnerReadable
  , setOwnerSearchable
  , setOwnerWritable
  )
import System.FilePath (joinPath)
import Control.Monad.State (runState)
import Control.Monad.Except (runExceptT)
import FileManager (performCommand)
import Types

fullSpec :: SpecWith ()
fullSpec = do
  performCommandSpec

performCommandSpec :: SpecWith ()
performCommandSpec = do
  describe "FileManager.performCommand" $ do
    it "cd works with simple path" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ ChangeDirectory "fold2/fold1") (testD, testD)
      res `shouldBe` (Right "")
      (Left cd) `shouldBe` (tData !! 3)
      rd `shouldBe`testD
    it "cd works with dots in path" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ ChangeDirectory "fold2/../fold3") (testD, testD)
      res `shouldBe` (Right "")
      (Left cd) `shouldBe` (tData !! 5)
      rd `shouldBe`testD
    it "cd returns error" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ ChangeDirectory "fold2/../..") (testD, testD)
      res `shouldBe` (Left $ InvalidPath "Path leads beyond FS.")
      cd `shouldBe`testD
      rd `shouldBe`testD

    it "create-file works" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let newFile = createFile "" "meow" ".txt" (joinPath ["users", "jarik", "testD", "meow.txt"])
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ CreateEmptyFile "meow.txt") (testD, testD)
      res `shouldBe` (Right $ "Created File with name: meow.txt\n")
      (getDirContent cd) `shouldContain` [Right newFile]
      rd `shouldBe`cd
    it "create-file returns error" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ CreateEmptyFile "../meow.txt") (testD, testD)
      res `shouldBe` (Left $ InvalidName "The following name is not valid: ../meow.txt")
      cd `shouldBe`testD
      rd `shouldBe`cd

    it "create-folder works" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let newDir = createDir [] "fold4" (joinPath ["users", "jarik", "testD", "fold4"])
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ CreateFolder "fold4") (testD, testD)
      res `shouldBe` (Right $ "Created Directory with name: fold4\n")
      (getDirContent cd) `shouldContain` [Left newDir]
      rd `shouldBe`cd
    it "create-folder returns error" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ CreateFolder "|hehe-boi|") (testD, testD)
      res `shouldBe` (Left $ InvalidName "The following name is not valid: |hehe-boi|")
      cd `shouldBe`testD
      rd `shouldBe`cd

    it "find-file successful" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (Right file3) = tData !! 2
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ FindFile "file3.txt") (testD, testD)
      res `shouldBe` (Right $ "Path to file is: " ++ (getFilePath file3) ++ "\n")
      cd `shouldBe`testD
      rd `shouldBe`cd
    it "find-file doesn't find" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ FindFile "i_dont_exist.lol") (testD, testD)
      res `shouldBe` (Left $ EntityNotFound "File with name i_dont_exist.lol not found.")
      cd `shouldBe`testD
      rd `shouldBe`cd

    it "file-info successful" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let file2 = tData !! 1
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ GetFileInfo "fold2/file2") (testD, testD)
      res `shouldBe` (Right $ getFSEInfo file2)
      cd `shouldBe`testD
      rd `shouldBe`cd
    it "file-info returns error" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ GetFileInfo "fold2/fold1/i_dont_exist.lol") (testD, testD)
      res `shouldBe` (Left $ EntityNotFound "File with name i_dont_exist.lol not found.")
      cd `shouldBe`testD
      rd `shouldBe`cd

    it "folder-info successful" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let fold1 = tData !! 3
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ GetFolderInfo "fold2/fold1") (testD, testD)
      res `shouldBe` (Right $ getFSEInfo fold1)
      cd `shouldBe`testD
      rd `shouldBe`cd
    it "folder-info returns error" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ GetFolderInfo "fold3/fold6") (testD, testD)
      res `shouldBe` (Left $ EntityNotFound "Directory with name fold6 not found.")
      cd `shouldBe`testD
      rd `shouldBe`cd

    it "write-file successful" $ do
      let expectedContent = "CONTENT"
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (Right file3) = tData !! 2
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ ModifyFile "fold2/fold1/file3" expectedContent) (testD, testD)
      res `shouldBe` (Right $ "Updated file with path " ++ getFilePath file3 ++ "\n")
      let (actualContent, _) = runState (runExceptT $ performCommand $ FS $ ShowFileContent "fold2/fold1/file3") (rd, cd)
      actualContent `shouldBe`(Right $ expectedContent ++ "\n")
      rd `shouldBe`cd
    it "write-file returns error" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ ModifyFile "fold3/fold6" "CONTENT") (testD, testD)
      res `shouldBe` (Left $ EntityNotFound "File with name fold6 not found.")
      cd `shouldBe`testD 
      rd `shouldBe`cd

    it "remove-file successful" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (Right file1) = tData !! 0
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ RemoveFile "fold2/../file1.txt") (testD, testD)
      let expectedCont = tail $ getDirContent testD
      res `shouldBe`(Right $ "Removed file with path " ++ getFilePath file1 ++ "\n")
      rd `shouldBe` (testD {getDirContent = expectedCont})
      rd `shouldBe`cd
    it "remove-file returns error" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ RemoveFile "fold2/fold1/meme.heh") (testD, testD)
      res `shouldBe` (Left $ EntityNotFound "File with name meme.heh not found.")
      cd `shouldBe`testD 
      rd `shouldBe`cd 

    it "remove-folder successful" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (Left fold3) = tData !! 5
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ RemoveFolder "fold2/../fold2/../fold3") (testD, testD)
      let expectedCont = init $ getDirContent testD
      res `shouldBe`(Right $ "Removed folder with path " ++ getDirPath fold3 ++ "\n")
      rd `shouldBe` (testD {getDirContent = expectedCont})
      rd `shouldBe`cd
    it "remove-folder returns error" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ RemoveFolder "fold2/fold1/meme_heh") (testD, testD)
      res `shouldBe` (Left $ EntityNotFound "Directory with name meme_heh not found.")
      cd `shouldBe`testD 
      rd `shouldBe`cd

    it "dir successful" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ ShowCurrentDirContent) (testD, testD)
      res `shouldBe`(Right $ "fold3\nfold2\nfile1\n")
      rd `shouldBe` testD
      rd `shouldBe`cd

    it "cat successful" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (Right file2) = tData !! 1
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ ShowFileContent "fold2/file2") (testD, testD)
      res `shouldBe`(Right $ getFileContent file2 ++ "\n")
      rd `shouldBe` testD
      rd `shouldBe`cd
    it "cat returns error" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ ShowFileContent "fold2/file5") (testD, testD)
      res `shouldBe` (Left $ EntityNotFound "File with name file5 not found.")
      cd `shouldBe`testD 
      rd `shouldBe`cd

    it "ls successful" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ ShowOtherDirContent "fold2/..") (testD, testD)
      res `shouldBe`(Right $ "fold3\nfold2\nfile1\n")
      rd `shouldBe` testD
      rd `shouldBe`cd
    it "ls returns error" $ do
      let tData = getTestData
      let (Left testD) = tData !! 6
      let (res, (rd, cd)) = runState (runExceptT $ performCommand $ FS $ ShowOtherDirContent "fold1/folder") (testD, testD)
      res `shouldBe` (Left $ EntityNotFound "Directory with name fold1 not found.")
      cd `shouldBe`testD 
      rd `shouldBe`cd

getTestData :: [FileSystemElement]
getTestData = 
  let 
    file1 = createFile "ababa" "file1" ".txt" (joinPath ["users", "jarik", "testD", "file1.txt"])
    file2 = createFile "<oh_god_why>" "file2" ".html" (joinPath ["users", "jarik", "testD", "fold2", "file2.html"])
    file3 = createFile "wow such content" "file3" ".txt" (joinPath ["users", "jarik", "testD", "fold2", "fold1", "file3.txt"])
    fold1 = createDir [Right file3] "fold1" (joinPath ["users", "jarik", "testD", "fold2", "fold1"])
    fold2 = createDir [Left fold1, Right file2] "fold2" (joinPath ["users", "jarik", "testD", "fold2"])
    fold3 = createDir [] "fold3" (joinPath ["users", "jarik", "testD", "fold3"])
    testD = createDir [Right file1, Left fold2, Left fold3] "testD" (joinPath ["users", "jarik", "testD"])
  in
    [Right file1, Right file2, Right file3, Left fold1, Left fold2, Left fold3, Left testD]

createFile :: FileContent -> FileName -> String -> FilePath -> File
createFile cont name ext path =
  File cont name (toInteger $ length cont) ext path defaultPermissions UndefinedTime

createDir :: [FileSystemElement] -> DirName -> FilePath -> Directory
createDir elems name path = Directory elems name path defaultPermissions

defaultPermissions :: Permissions
defaultPermissions = setOwnerSearchable True $ 
  setOwnerReadable True $ 
  setOwnerWritable True $ emptyPermissions