module ParserSpec 
  ( fullSpec
  ) where

import Test.Hspec 
  ( SpecWith
  , describe
  , it
  , shouldBe
  )
import Parser (parseCommand)
import Types

fullSpec :: SpecWith ()
fullSpec = do
  simpleCommandSpec
  fsCommandSpec
  vcsCommandSpec

simpleCommandSpec :: SpecWith ()
simpleCommandSpec = do
  describe "Simple commands parser" $ do
    it "Correct exit command" $ do
      parseCommand "exit" `shouldBe` Just Exit
    it "Incorrect exit command" $ do
      parseCommand "exit plz" `shouldBe` Nothing
    it "Correct help command" $ do
      parseCommand "help" `shouldBe` Just Help
    it "Incorrect help command" $ do
      parseCommand "halp me" `shouldBe` Nothing

fsCommandSpec :: SpecWith ()
fsCommandSpec = do
  describe "File system command parser" $ do
    it "Correct cd with relative path command" $ do
      parseCommand "cd some_dir/dir" `shouldBe` (Just $ FS $ ChangeDirectory "some_dir/dir")
    it "Correct cd with absolute path command" $ do
      parseCommand "cd C:\\Users\\jarik\\Desktop" 
        `shouldBe` (Just $ FS $ ChangeDirectory "C:\\Users\\jarik\\Desktop")
    it "Correct cd with dots in path command" $ do
      parseCommand "cd ../../dir" `shouldBe` (Just $ FS $ ChangeDirectory "../../dir")
    it "Incorrect cd command" $ do
      parseCommand "cd nope nope" `shouldBe` Nothing

    it "Correct create-file command" $ do
      parseCommand "create-file name" `shouldBe` (Just $ FS $ CreateEmptyFile "name")
    it "Incorrect create-file command" $ do
      parseCommand "craete-file mem" `shouldBe` Nothing

    it "Correct create-folder command" $ do
      parseCommand "create-folder name" `shouldBe` (Just $ FS $ CreateFolder "name")
    it "Incorrect create-folder command" $ do
      parseCommand "create-folber mem" `shouldBe` Nothing

    it "Correct find-file command" $ do
      parseCommand "find-file namie" `shouldBe` (Just $ FS $ FindFile "namie")
    it "Incorrect find-file command" $ do
      parseCommand "find-file mem ahah" `shouldBe` Nothing

    it "Correct file-info command" $ do
      parseCommand "file-info namie" `shouldBe` (Just $ FS $ GetFileInfo "namie")
    it "Incorrect file-info command" $ do
      parseCommand "file-infoo memes" `shouldBe` Nothing

    it "Correct folder-info command" $ do
      parseCommand "folder-info namie" `shouldBe` (Just $ FS $ GetFolderInfo "namie")
    it "Incorrect folder-info command" $ do
      parseCommand "folber-info memes" `shouldBe` Nothing

    it "Correct write-file command" $ do
      parseCommand "write-file pathie content" `shouldBe` (Just $ FS $ ModifyFile "pathie" "content")
    it "Incorrect write-file command" $ do
      parseCommand "write-file mem_path_no_content" `shouldBe` Nothing

    it "Correct remove-file command" $ do
      parseCommand "remove-file path" `shouldBe` (Just $ FS $ RemoveFile "path")
    it "Incorrect remove-file command" $ do
      parseCommand "remove-file" `shouldBe` Nothing

    it "Correct remove-folder command" $ do
      parseCommand "remove-folder path/../qwe" `shouldBe` (Just $ FS $ RemoveFolder "path/../qwe")
    it "Incorrect remove-folder command" $ do
      parseCommand "remove-folderr folder" `shouldBe` Nothing

    it "Correct dir command" $ do
      parseCommand "dir" `shouldBe` (Just $ FS ShowCurrentDirContent)
    it "Incorrect dir command" $ do
      parseCommand "dir folder" `shouldBe` Nothing

    it "Correct cat command" $ do
      parseCommand "cat tfile/path/file.txt" `shouldBe` (Just $ FS $ ShowFileContent "tfile/path/file.txt")
    it "Incorrect cat command" $ do
      parseCommand "cat is nice" `shouldBe` Nothing

    it "Correct ls command" $ do
      parseCommand "ls path/to/dir" `shouldBe` (Just $ FS $ ShowOtherDirContent "path/to/dir")
    it "Incorrect ls command" $ do
      parseCommand "lss go brrr" `shouldBe` Nothing

vcsCommandSpec :: SpecWith ()
vcsCommandSpec = do
  describe "VCS command parser" $ do
    it "Correct vcs-add command" $ do
      parseCommand "vcs-add filpath" `shouldBe` (Just $ VCS $ Add "filpath")
    it "Incorrect vcs-add command" $ do
      parseCommand "vcs-add all system" `shouldBe` Nothing

    it "Correct vcs-delete-version command" $ do
      parseCommand "vcs-delete-version path 10" `shouldBe` (Just $ VCS $ DeleteFileRevision "path" "10")
    it "Incorrect vcs-delete-version command" $ do
      parseCommand "vcs-delete-versian all 100" `shouldBe` Nothing

    it "Correct vcs-init command" $ do
      parseCommand "vcs-init" `shouldBe` (Just $ VCS Initialize)
    it "Incorrect vcs-init command" $ do
      parseCommand "vcs-init here" `shouldBe` Nothing

    it "Correct vcs-merge command" $ do
      parseCommand "vcs-merge path 0 1 both" `shouldBe` (Just $ VCS $ Merge "path" "0" "1" BothStrategy)
    it "Incorrect vcs-merge command" $ do
      parseCommand "vcs-merge path 0 1 left-and-right" `shouldBe` Nothing

    it "Correct vcs-remove command" $ do
      parseCommand "vcs-remove filepath" `shouldBe` (Just $ VCS $ RemoveFromVCS "filepath")
    it "Incorrect vcs-remove command" $ do
      parseCommand "vcs-remove filepath and it" `shouldBe` Nothing

    it "Correct vcs-history command" $ do
      parseCommand "vcs-history /dir/dor/dar" `shouldBe` (Just $ VCS $ ShowFileChangeHistory "/dir/dor/dar")
    it "Incorrect vcs-history command" $ do
      parseCommand "vcs-historyy" `shouldBe` Nothing

    it "Correct vcs-cat command" $ do
      parseCommand "vcs-cat this 5" `shouldBe` (Just $ VCS $ ShowFileRevision "this" "5")
    it "Incorrect vcs-cat command" $ do
      parseCommand "vcs-cat that 10 and 4" `shouldBe` Nothing

    it "Correct vcs-show-all command" $ do
      parseCommand "vcs-show-all" `shouldBe` (Just $ VCS ShowVCSHistory)
    it "Incorrect vcs-show-all command" $ do
      parseCommand "vcs-show-all C:\\Users\\Desktop" `shouldBe` Nothing

    it "Correct vcs-update command" $ do
      parseCommand "vcs-update here/is/file yay" `shouldBe` (Just $ VCS $ Update "here/is/file" "yay")
    it "Incorrect vcs-update command" $ do
      parseCommand "vcs-update " `shouldBe` Nothing
