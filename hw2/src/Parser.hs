module Parser
  ( parseCommand
  ) where

import Options.Applicative
  ( Parser
  , ParserResult (..)
  , ParserInfo
  , ReadM

  , execParserPure
  , defaultPrefs
  
  , (<|>)

  , argument
  , command
  , eitherReader
  , helper
  , info
  , metavar
  , progDesc
  , subparser
  , str
  )
import Types 
  ( Command (..)
  , FSCommand (..)
  , VCSCommand (..)
  , VCSMergeStrategy (..)
  )

---------------------------------------------------------------------------------------------------

-- | Parses given string into Command. Returns Nothing on failure.
parseCommand :: String -> Maybe Command
parseCommand s = 
  let 
    ws = words s
    res =
      execParserPure defaultPrefs (parseCommand' `withInfo` "Interact with FileSystem and VCS") ws
  in
    case res of 
      Success a -> Just a
      _ -> Nothing

parseCommand' :: Parser Command
parseCommand' = parseFSCommand
  <|> parseSimpleCommand
  <|> parseVCSCommand

---------------------------------------------------------------------------------------------------

parseSimpleCommand :: Parser Command
parseSimpleCommand = subparser $
  command "help" (parseHelp `withInfo` "Show help") <>
  command "exit" (parseExit `withInfo` "Close application")

parseExit :: Parser Command
parseExit = pure Exit

parseHelp :: Parser Command
parseHelp = pure Help

---------------------------------------------------------------------------------------------------

parseFSCommand :: Parser Command
parseFSCommand = FS <$> parseFSCommand'

parseFSCommand' :: Parser FSCommand
parseFSCommand' = subparser $
  command "cd" (parseChangeDirectory `withInfo` "Move to specified directory") <>
  command "create-file" (parseCreateEmptyFile `withInfo` "Create empty file") <>
  command "create-folder" (parseCreateFolder `withInfo` "Create empty folder") <>
  command "find-file" (parseFindFile `withInfo` "Search for file by name") <>
  command "file-info" (parseGetFileInfo `withInfo` "Show info about file") <>
  command "folder-info" (parseGetFolderInfo `withInfo` "Show info about directory") <>
  command "write-file" (parseModifyFile `withInfo` "Write text to the file") <>
  command "remove-file" (parseRemoveFile `withInfo` "Remove file") <>
  command "remove-folder" (parseRemoveFolder `withInfo` "Remove directory") <>
  command "dir" (parseShowCurrentDirContent `withInfo` "Show content of current directory") <>
  command "cat" (parseShowFileContent `withInfo` "Show file content") <>
  command "ls" (parseShowOtherDirContent `withInfo` "Show content of specified directory")

parseChangeDirectory :: Parser FSCommand
parseChangeDirectory = ChangeDirectory
  <$> argument str (metavar "<folder-path>")

parseCreateEmptyFile :: Parser FSCommand
parseCreateEmptyFile = CreateEmptyFile
  <$> argument str (metavar "\"file-name\"")

parseCreateFolder :: Parser FSCommand
parseCreateFolder = CreateFolder
  <$> argument str (metavar "\"folder-name\"")

parseFindFile :: Parser FSCommand
parseFindFile = FindFile
  <$> argument str (metavar "\"file-name\"")

parseGetFileInfo :: Parser FSCommand
parseGetFileInfo = GetFileInfo
  <$> argument str (metavar "<file-path>")

parseGetFolderInfo :: Parser FSCommand
parseGetFolderInfo = GetFolderInfo
  <$> argument str (metavar "<folder-path>")  

parseModifyFile :: Parser FSCommand
parseModifyFile = ModifyFile
  <$> argument str (metavar "<file-path>")
  <*> argument str (metavar "\"text\"")

parseRemoveFile :: Parser FSCommand
parseRemoveFile = RemoveFile
  <$> argument str (metavar "<file-path>")

parseRemoveFolder :: Parser FSCommand
parseRemoveFolder = RemoveFolder
  <$> argument str (metavar "<folder-path>")

parseShowCurrentDirContent :: Parser FSCommand
parseShowCurrentDirContent = pure ShowCurrentDirContent

parseShowFileContent :: Parser FSCommand
parseShowFileContent = ShowFileContent
  <$> argument str (metavar "<file-path>")

parseShowOtherDirContent :: Parser FSCommand
parseShowOtherDirContent = ShowOtherDirContent
  <$> argument str (metavar "<folder-path>")

---------------------------------------------------------------------------------------------------

parseVCSCommand :: Parser Command
parseVCSCommand = VCS <$> parseVCSCommand'

parseVCSCommand' :: Parser VCSCommand
parseVCSCommand' = subparser $
  command "vcs-add" (parseAdd `withInfo` "Add file or directory (all files inside) to VCS") <>
  command "vcs-delete-version" (parseDeleteFileRevision `withInfo` "Remove version from VCS") <>
  command "vcs-init" (parseInitialize `withInfo` "Initialize VCS in current directory") <>
  command "vcs-merge" (parseMerge `withInfo` "Merge two versions of file") <>
  command "vcs-remove" (parseRemoveFromVCS `withInfo` "Remove file from VCS") <>
  command "vcs-history" (parseShowFileChangeHistory `withInfo` "Show file editing history") <>
  command "vcs-cat" (parseShowFileRevision `withInfo` "Show specified file version") <>
  command "vcs-show-all" (parseShowVCSHistory `withInfo` "Show whole history of VCS") <>
  command "vcs-update" (parseUpdate `withInfo` "Add file changes to VCS")

parseAdd :: Parser VCSCommand
parseAdd = Add
  <$> argument str (metavar "<folder-path | file-path>")

parseDeleteFileRevision :: Parser VCSCommand
parseDeleteFileRevision = DeleteFileRevision
  <$> argument str (metavar "<file-path>")
  <*> argument str (metavar "\"index\"")

parseInitialize :: Parser VCSCommand
parseInitialize = pure Initialize

parseMerge :: Parser VCSCommand
parseMerge = Merge
  <$> argument str (metavar "<file-path>")
  <*> argument str (metavar "\"index1\"")
  <*> argument str (metavar "\"index2\"")
  <*> argument mergeStrategy (metavar "left | right | both")

parseRemoveFromVCS :: Parser VCSCommand
parseRemoveFromVCS = RemoveFromVCS
  <$> argument str (metavar "<file-path>")

parseShowFileChangeHistory :: Parser VCSCommand
parseShowFileChangeHistory = ShowFileChangeHistory
  <$> argument str (metavar "<file-path>")

parseShowFileRevision :: Parser VCSCommand
parseShowFileRevision = ShowFileRevision
  <$> argument str (metavar "<file-path>")
  <*> argument str (metavar "\"index\"")

parseShowVCSHistory :: Parser VCSCommand
parseShowVCSHistory = pure ShowVCSHistory

parseUpdate :: Parser VCSCommand
parseUpdate = Update
  <$> argument str (metavar "<file-path>")
  <*> argument str (metavar "\"comment\"")

---------------------------------------------------------------------------------------------------

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

mergeStrategy :: ReadM VCSMergeStrategy
mergeStrategy = eitherReader parseVCSMergeStrategy

parseVCSMergeStrategy :: String -> Either String VCSMergeStrategy
parseVCSMergeStrategy s = case s of
  "left"  -> Right LeftStrategy
  "right" -> Right RightStrategy
  "both"  -> Right BothStrategy
  _       -> Left "Unknown merge strategy"