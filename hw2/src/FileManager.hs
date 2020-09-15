{-# LANGUAGE InstanceSigs #-}

module FileManager
  ( buildFileSystem 
  , performCommand
  , saveFileSystemOnDisk

  , getPrefix
  , printHelp
  ) where

import Data.Maybe (fromJust)
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.State (State, get, put)
import Control.Monad.Except (ExceptT, lift, throwError)
import Control.Exception (bracket, throw)
import System.IO 
  ( IOMode (..)
  
  , openFile
  , hClose
  , hPutStr
  )
import System.Directory 
  ( Permissions (..)

  , emptyPermissions
  , setOwnerReadable
  , setOwnerSearchable
  , setOwnerWritable

  , createDirectory
  , doesDirectoryExist
  , doesFileExist
  , getFileSize
  , getModificationTime
  , getPermissions
  , listDirectory
  , removeFile
  , removeDirectoryRecursive
  )
import System.FilePath 
  ( (</>)
  , dropTrailingPathSeparator
  , isAbsolute
  , isValid
  , splitPath
  , takeExtension
  , takeBaseName
  )
import Types
  ( Directory (..)
  , File (..)
  , FileManagerException (..)
  , FileSystemTime (..)

  , Command (..)
  , FSCommand (..)

  , DirName
  , FileContent
  , FileName
  , FileSystemElement

  , CommandResult
  , FSState
  , RootDirectory

  , getFSEInfo
  )

data EntityType 
  = FileEntity
  | DirectoryEntity
  deriving (Eq)

instance Show EntityType where
  show :: EntityType -> String
  show ent = case ent of
    FileEntity -> "File"
    DirectoryEntity -> "Directory"

---------------------------------------------------------------------------------------------------

-- | Get command line prefix
getPrefix :: FSState -> String
getPrefix (_, cd) = getDirPath cd ++ "> "


-- | Print help page.
printHelp :: IO ()
printHelp = do
  putStrLn "NOTE: absolute path means 'absolute in real file system'"
  putStrLn "help - print this help message"
  putStrLn "exit - close app and save all changes on disk"
  putStrLn 
    "cd <path> - change directory. Path must be absolute or relative (you can use .. in relative)"
  putStrLn "create-file <name> - create empty file in current directory"
  putStrLn "create-folder <name> - create empty folder in current directory"
  putStrLn "find-file <name> - search for file in current directory and all subdirectories"
  putStrLn "file-info <path> - get info about file by path (absolute or relative)"
  putStrLn "folder-info <path> - get info about folder by path (absolute or relative)"
  putStrLn "write-file <path> <text> - change file content to given text"
  putStrLn "remove-file <path> - delete file by path"
  putStrLn "remove-folder <path> - delete folder and all content within it"
  putStrLn "dir - show current directory content"
  putStrLn "cat <path> - show file content"
  putStrLn "ls <path> - show other directory content"
  putStrLn "VCS commands are not supported"

---------------------------------------------------------------------------------------------------

-- | Saves changes on disk. Takes old version of file system
-- and new one.
saveFileSystemOnDisk :: Directory -> Directory -> IO ()
saveFileSystemOnDisk old new = do
  let oldContent = getDirContent old
  let newContent = getDirContent new
  toDelete <- helper newContent oldContent
  forM_ toDelete removeEntityFromDisk
  where
    helper :: [FileSystemElement] -> [FileSystemElement] -> IO [FileSystemElement]
    helper (n:ns) oldToProceed = do
      nextOld <- chooseDestinyOfElement n [] oldToProceed
      helper ns nextOld
    helper [] oldToProceed = return oldToProceed

-- | Help function to saveFS. Chooses what to do with element: delete, modify, create.
chooseDestinyOfElement :: FileSystemElement -> [FileSystemElement] -> [FileSystemElement]
  -> IO [FileSystemElement] 
chooseDestinyOfElement element rest [] = do
  createEntityOnDisk element
  return rest
chooseDestinyOfElement (Left dir) rest (c:cs) = do
  case c of
    Left oldDir -> 
      if getDirPath dir == getDirPath oldDir
      then do
        saveFileSystemOnDisk oldDir dir
        return $ rest ++ cs
      else chooseDestinyOfElement (Left dir) (rest ++ [c]) cs
    Right _ -> chooseDestinyOfElement (Left dir) (rest ++ [c]) cs
chooseDestinyOfElement (Right file) rest (c:cs) = do
  case c of
    Left _ -> chooseDestinyOfElement (Right file) (rest ++ [c]) cs
    Right oldFile ->
      if getFilePath file == getFilePath oldFile
      then do
        removeFile $ getFilePath oldFile
        createEntityOnDisk (Right file)
        return $ rest ++ cs
      else chooseDestinyOfElement (Right file) (rest ++ [c]) cs

-- | Creates file/folder in real file system.
createEntityOnDisk :: FileSystemElement -> IO ()
createEntityOnDisk (Left dir) = do
  createDirectory $ getDirPath dir
  forM_ (getDirContent dir) createEntityOnDisk
createEntityOnDisk (Right file) = do
  bracket 
    (openFile (getFilePath file) WriteMode)
    (hClose)
    (\outh -> hPutStr outh (getFileContent file))

-- | Deletes file/folder from real file system.
removeEntityFromDisk :: FileSystemElement -> IO ()
removeEntityFromDisk (Left dir) = removeDirectoryRecursive $ getDirPath dir
removeEntityFromDisk (Right file) = removeFile $ getFilePath file

---------------------------------------------------------------------------------------------------

-- | Builds virtual file system, given folder will be new root.
buildFileSystem :: FilePath -> IO RootDirectory
buildFileSystem rootPath = do
  let normalRootPath = dropTrailingPathSeparator rootPath
  rootName <- getDirNameByPath normalRootPath
  let rootDir = createEmptyDir rootName normalRootPath
  rootContentRaw <- listDirectoryWithAbsPath normalRootPath
  fillDirWithContent rootDir rootContentRaw

-- | Translates real files/folders into virtual analogues and adds it
-- to given Directory.
fillDirWithContent :: Directory -> [FilePath] -> IO Directory
fillDirWithContent dir []     = return dir
fillDirWithContent dir (c:cs) = do
  isElemAFile <- isFile c
  if isElemAFile
  then do
    file <- createFileFromRaw c
    let nDir = addFSEToDir dir (Right file)
    fillDirWithContent nDir cs 
  else do
    isElemADir <- isDir c
    if isElemADir
    then do
      subdirEmpty <- createDirFromRaw c
      subdirContent <- listDirectoryWithAbsPath c
      subdir <- fillDirWithContent subdirEmpty subdirContent
      let nDir = addFSEToDir dir (Left subdir)
      fillDirWithContent nDir cs
    else throw $ UnknownEntity "Encountered unknown entity (not file or folder). Aborting."

-- | Translate real file into virtual representation.
createFileFromRaw :: FilePath -> IO File
createFileFromRaw fp = do
  let fName = takeBaseName fp
  let fType = takeExtensionOrDefault fp
  fPerm <- getPermissions fp
  fModTime <- getModificationTime fp
  fContent <- readFile fp
  fSize <- getFileSize fp
  return $ File fContent fName fSize fType fp fPerm (Time fModTime)

-- | Translate real folder into empty virtual representation.
createDirFromRaw :: FilePath -> IO Directory
createDirFromRaw fp = do
  dirName <- getDirNameByPath fp
  dirPerm <- getPermissions fp
  return $ createEmptyDirWithPermissions dirName fp dirPerm

-- | Adds File/Directory to given Directory. 
addFSEToDir :: Directory -> FileSystemElement -> Directory
addFSEToDir dir@(Directory { getDirContent = old }) fse = dir { getDirContent = fse : old }

-- | Deletes File/Directory from given Directory. 
deleteFSEFromDir :: Directory -> FileSystemElement -> Directory
deleteFSEFromDir dir@(Directory { getDirContent = old }) fse = 
  dir { getDirContent = (filter (/= fse) old) }

-- | Replaces old File/Directory with a new one in given Directory.
exchangeFSEInDir :: Directory -> FileSystemElement -> FileSystemElement -> Directory
exchangeFSEInDir dir old new = addFSEToDir (deleteFSEFromDir dir old) new

-- | Modifies File content.
modifyFile :: File -> FileContent -> File
modifyFile file content = 
  let
    newSize = toInteger $ length content
  in 
    file 
      { 
        getFileContent = content
      , getFileSizeBytes = newSize
      , getFileModifTime = UndefinedTime
      }

-- | Extracts folder name from path to it.
getDirNameByPath :: FilePath -> IO DirName
getDirNameByPath fp
  | isValidPath fp = return $ last $ splitPathAndDropTrailSep fp
  | otherwise  = throw $ InvalidPath $ "The following path is not valid: " ++ fp

---------------------------------------------------------------------------------------------------

-- | Performs given command within our virtual file system and returns result of 
-- operation on success or exception on error. 
performCommand :: Command -> ExceptT FileManagerException (State FSState) CommandResult
performCommand cmd = case cmd of
  FS fscmd -> case fscmd of
    ShowCurrentDirContent -> do
      (_, cd) <- lift get
      return $ getDirContentAsStr cd
    ShowOtherDirContent path -> do
      fse <- getEntityByPath path DirectoryEntity
      let (Left dir) = fse
      return $ getDirContentAsStr dir
    ShowFileContent path -> do
      fse <- getEntityByPath path FileEntity
      let (Right file) = fse
      return $ getFileContent file ++ "\n"
    CreateEmptyFile name -> createEmptyEntity FileEntity name
    CreateFolder name -> createEmptyEntity DirectoryEntity name
    FindFile name -> do
      (_, cd) <- lift get 
      case findFileByName cd name of
        Just file -> return $ "Path to file is: " ++ getFilePath file ++ "\n"
        Nothing -> throwError $ EntityNotFound $ "File with name " ++ name ++ " not found."
    ChangeDirectory path -> do
      fse <- getEntityByPath path DirectoryEntity
      let (Left dir) = fse 
      (rd, _) <- lift get
      lift $ put (rd, dir)
      return ""
    GetFileInfo path -> do
      fse <- getEntityByPath path FileEntity
      return $ getFSEInfo fse 
    GetFolderInfo path -> do
      fse <- getEntityByPath path DirectoryEntity
      return $ getFSEInfo fse
    RemoveFile path -> do
      fse <- getEntityByPath path FileEntity
      let (Right file) = fse
      (rd, _) <- lift get
      case getParentDir rd fse of
        Just dir -> do
          let nDir = deleteFSEFromDir dir fse
          updateDir dir nDir
          return $ "Removed file with path " ++ getFilePath file ++ "\n"
        Nothing -> throwError $ EntityNotFound $ "File with path " ++ getFilePath file ++ " not found."
    RemoveFolder path -> do
      fse <- getEntityByPath path DirectoryEntity
      let (Left dir) = fse
      (rd, _) <- lift get
      case getParentDir rd fse of
        Just old -> do
          moveCdPointer dir
          let nDir = deleteFSEFromDir old fse
          updateDir old nDir
          return $ "Removed folder with path " ++ getDirPath dir ++ "\n"
        Nothing -> throwError $ UnsupportedOperation $ "Root folder cannot be deleted."
    ModifyFile path content -> do
      fse <- getEntityByPath path FileEntity
      let (Right file) = fse
      let nFile = modifyFile file content
      (rd, _) <- lift get
      case getParentDir rd fse of
        Just dir -> do
          let nDir = exchangeFSEInDir dir fse (Right nFile)
          updateDir dir nDir
          return $ "Updated file with path " ++ getFilePath file ++ "\n"
        Nothing -> throwError $ EntityNotFound $ "File with path " ++ getFilePath file ++ " not found."
  ucmd -> throwError $ UnsupportedOperation $ "Command " ++ show ucmd ++ "not supported."

-- | Search for File/Directory in the whole file system by given path.
-- Fails if path isn't valid, leads beyond file system or to
-- nonexistent entity.
getEntityByPath :: FilePath -> EntityType 
  -> ExceptT FileManagerException (State FSState) FileSystemElement
getEntityByPath fp eType
  | isValidPath fp =
    if isAbsolute fp
    then do
      rel <- makeRelative fp
      (rd, _) <- lift get
      traverseFS rd rel eType
    else do
      (_, cd) <- lift get
      traverseFS cd (splitPathAndDropTrailSep fp) eType
  | otherwise = throwError $ InvalidPath $ "The following path is not valid: " ++ fp

-- | Help function for getEntityByPath. Traversing virtual file system, where start is
-- given Directory. 
traverseFS :: Directory -> [FilePath] -> EntityType 
  -> ExceptT FileManagerException (State FSState) FileSystemElement
traverseFS cd [] eType = 
  if eType == DirectoryEntity
  then return $ Left cd
  else throwError $ EntityNotFound $ show eType ++ " not found."
traverseFS cd ("..":xs) eType = do
  (rd, _) <- lift get
  let parentDir = getParentDir rd (Left cd)
  case parentDir of
    Just dir -> traverseFS dir xs eType
    Nothing -> throwError $ InvalidPath $ "Path leads beyond FS."
traverseFS cd (x:[]) eType = do
  res <- findEntityWithinContentByName x (getDirContent cd) eType
  case res of
    Just entity -> return entity
    Nothing -> throwError $ EntityNotFound $ show eType ++ " with name " ++ x ++ " not found."
traverseFS cd (x:xs) eType = do
  res <- findEntityWithinContentByName x (getDirContent cd) DirectoryEntity
  case res of
    Just (Left dir) -> traverseFS dir xs eType
    _ -> throwError $ EntityNotFound $ "Directory with name " ++ x ++ " not found."

-- | Search for File/Directory by name in given directory's content.
findEntityWithinContentByName :: String -> [FileSystemElement] -> EntityType
  -> ExceptT FileManagerException (State FSState) (Maybe FileSystemElement)
findEntityWithinContentByName _ [] _ = return Nothing
findEntityWithinContentByName name (c:cs) eType = case c of
  Right file -> 
    if eType == FileEntity && checkFileName file name
    then return $ Just $ Right file
    else findEntityWithinContentByName name cs eType
  Left dir ->
    if eType == DirectoryEntity && (getDirName dir) == name
    then return $ Just $ Left dir
    else findEntityWithinContentByName name cs eType

-- | Makes absolute path relative to the root of virtual file system.
makeRelative :: FilePath -> ExceptT FileManagerException (State FSState) [FilePath]
makeRelative fp = do
  (rd, _) <- lift get
  let rootPath = getDirPath rd
  let splitedRootPath = splitPathAndDropTrailSep rootPath
  let splitedPath = splitPathAndDropTrailSep fp
  helper splitedRootPath splitedPath
    where
      helper :: [FilePath] -> [FilePath] -> ExceptT FileManagerException (State FSState) [FilePath]
      helper [] [] = return []
      helper [] y  = return y
      helper _ []  = throwError $ InvalidPath $ "This path leads beyond FS: " ++ fp
      helper (x:root) (y:rel) = 
        if x == y
        then helper root rel
        else throwError $ InvalidPath $ "This path leads beyond FS: " ++ fp

-- | Creates empty File/Directory with given name in CurrentDirectory.
createEmptyEntity :: EntityType -> String 
  -> ExceptT FileManagerException (State FSState) CommandResult
createEmptyEntity eType name
  | isValidFileName name = do
    (_, cd) <- lift get
    entity <- case eType of
      FileEntity -> return $ Right $ createEmptyFile name (getDirPath cd </> name) UndefinedTime
      DirectoryEntity -> return $ Left $ createEmptyDir name (getDirPath cd </> name)
    let nCd = addFSEToDir cd entity
    updateDir cd nCd
    return $ "Created " ++ show eType ++ " with name: " ++ name ++ "\n"
  | otherwise = throwError $ InvalidName $ "The following name is not valid: " ++ name

-- | Replaces old Directory with a new one and updates all parent directories, RootDirectoty
-- and CurrentDirectory (if needed)
updateDir :: Directory -> Directory -> ExceptT FileManagerException (State FSState) ()
updateDir old new = do
  (rd, cd) <- lift get
  if old == cd && old == rd
  then do
    lift $ put (new, new)
    return ()
  else 
    if old == cd
    then do
      lift $ put (rd, new)
      let parent = fromJust $ getParentDir rd (Left old)
      let nParent = exchangeFSEInDir parent (Left old) (Left new)
      updateDir parent nParent
    else 
      if old == rd
      then do
        lift $ put (new, cd)
        return ()
      else do
        let parent = fromJust $ getParentDir rd (Left old)
        let nParent = exchangeFSEInDir parent (Left old) (Left new)
        updateDir parent nParent

-- | Changes CurrentDirectory to RootDirectory if it must be deleted.
moveCdPointer :: Directory -> ExceptT FileManagerException (State FSState) ()
moveCdPointer dir = do
  (rd, cd) <- lift get
  case findDirByObj dir cd of
    Nothing -> return ()
    Just _ -> lift $ put (rd, rd)

-- | Returns Directory content as formatted string. 
getDirContentAsStr :: Directory -> String
getDirContentAsStr dir = foldr folder "" $ map mapper $ getDirContent dir
  where
    mapper :: FileSystemElement -> String
    mapper (Left d)   = getDirName d
    mapper (Right f) = getFileName f

    folder :: String -> CommandResult -> CommandResult
    folder str res = res ++ str ++ "\n"

-- | Searches for file by name in the whole virtual file system.
findFileByName :: RootDirectory -> FileName -> Maybe File
findFileByName rd name = helper (getDirContent rd)
  where
    helper :: [FileSystemElement] -> Maybe File
    helper [] = Nothing
    helper (c:cs) = case c of
      Right file -> 
        if checkFileName file name
        then Just $ file
        else helper cs
      Left dir -> (helper cs) <|> findFileByName dir name

findDirByObj :: RootDirectory -> Directory -> Maybe Directory
findDirByObj rd dir = helper (getDirContent rd)
  where
    helper :: [FileSystemElement] -> Maybe Directory
    helper [] = Nothing
    helper (c:cs) = case c of
      Right _ -> helper cs
      Left cd ->
        if dir == cd
        then Just cd
        else helper cs <|> findDirByObj cd dir

-- | Returns parent Directory to given element.
getParentDir :: Directory -> FileSystemElement -> Maybe Directory
getParentDir cd element = helper (getDirContent cd)
  where
    helper :: [FileSystemElement] -> Maybe Directory
    helper [] = Nothing
    helper (c:cs) = 
      if c == element
      then Just cd
      else case c of
        Left dir -> (helper cs) <|> getParentDir dir element
        Right _ -> helper cs

---------------------------------------------------------------------------------------------------

-- | Creates empty File with default permissions
createEmptyFile :: FileName -> FilePath -> FileSystemTime -> File
createEmptyFile name path time =
  File "" (takeBaseName name) 0 (takeExtensionOrDefault name) path defaultPermissions time

-- | Creates empty Directory with default permissions
createEmptyDir :: DirName -> FilePath -> Directory
createEmptyDir name path = createEmptyDirWithPermissions name path defaultPermissions

-- | Creates empty Directory with specified permissions
createEmptyDirWithPermissions :: DirName -> FilePath -> Permissions -> Directory
createEmptyDirWithPermissions n fp p = Directory [] n fp p

-- | Default permissions for new Files/Directories
defaultPermissions :: Permissions
defaultPermissions = setOwnerSearchable True $ 
  setOwnerReadable True $ 
  setOwnerWritable True $ emptyPermissions

---------------------------------------------------------------------------------------------------

-- | Returns list of absolute paths to real files/folders 
-- within specified folder.
listDirectoryWithAbsPath :: FilePath -> IO [FilePath]
listDirectoryWithAbsPath path = do
  paths <- listDirectory path
  return $ map (path </>) paths

-- | Returns file extension or default value if it
-- doesn't have one.
takeExtensionOrDefault :: FileName -> String
takeExtensionOrDefault fp = case takeExtension fp of
  "" -> "No extension"
  s  -> s

-- | Helps to search file by only name or by name and extension.
checkFileName :: File -> FileName -> Bool
checkFileName file name = 
  getFileName file == name || (getFileName file) ++ (getFileType file) == name

-- | Checks is given path leads to a file.
isFile :: FilePath -> IO Bool
isFile = doesFileExist

-- | Checks is given path leads to a folder.
isDir :: FilePath -> IO Bool
isDir = doesDirectoryExist

-- | Validate path.
isValidPath :: FilePath -> Bool
isValidPath = isValid

-- | Validate file name.
isValidFileName :: FileName -> Bool
isValidFileName f = not $ foldr ((||) . isForbiddenChar) False f

-- | Checks is char forbidden to use.
isForbiddenChar :: Char -> Bool
isForbiddenChar c = elem c ['\\', '/', '|', ':', '"', '*', '?', '<', '>']

-- | Splits path and removes trailing separators.
splitPathAndDropTrailSep :: FilePath -> [FilePath]
splitPathAndDropTrailSep fp = map dropTrailingPathSeparator (splitPath fp)