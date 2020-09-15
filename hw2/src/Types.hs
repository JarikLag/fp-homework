{-# LANGUAGE InstanceSigs #-}

module Types
  ( Command (..)
  , FSCommand (..)
  , VCSCommand (..)
  , VCSMergeStrategy (..)

  , Directory (..)
  , File (..)
  , FileManagerException (..)
  , FileSystemTime (..)
  
  , DirName
  , FileContent
  , FileName
  , FileSystemElement 
  , VCSComment
  , VCSRevisionIndex

  , CommandResult
  , CurrentDirectory
  , FSState
  , RootDirectory

  , getFSEInfo
  ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Time.Clock (UTCTime (..))
import System.Directory (Permissions (..))

-- | General type for commands.
data Command
  = Exit
  | Help
  | FS FSCommand
  | VCS VCSCommand
  deriving (Eq, Show)

-- | Type to represent commands to work with file system.
data FSCommand 
  = ChangeDirectory FilePath
  | CreateEmptyFile FileName
  | CreateFolder DirName
  | FindFile FileName
  | GetFileInfo FilePath
  | GetFolderInfo FilePath
  | ModifyFile FilePath FileContent
  | RemoveFile FilePath
  | RemoveFolder FilePath
  | ShowCurrentDirContent
  | ShowFileContent FilePath
  | ShowOtherDirContent FilePath
  deriving (Eq, Show)

-- | Type to represent commands to work with VCS.
data VCSCommand
  = Add FilePath
  | DeleteFileRevision FilePath VCSRevisionIndex
  | Initialize
  | Merge FilePath VCSRevisionIndex VCSRevisionIndex VCSMergeStrategy
  | RemoveFromVCS FilePath
  | ShowFileChangeHistory FilePath
  | ShowFileRevision FilePath VCSRevisionIndex
  | ShowVCSHistory
  | Update FilePath VCSComment
  deriving (Eq, Show)

-- | Merge strategies for vcs-merge. Left takes content from
-- first file, right from second file, both will concatenate
-- content from files.  
data VCSMergeStrategy
  = LeftStrategy
  | RightStrategy
  | BothStrategy
  deriving (Show, Eq)

type FileContent = String
type FileName = String
type DirName = String
type VCSComment = String
type VCSRevisionIndex = String

type CommandResult = String
type RootDirectory = Directory
type CurrentDirectory = Directory
type FSState = (RootDirectory, CurrentDirectory)

-- | Type to represent real file in virtual file system.
data File = File 
  { getFileContent     :: FileContent
  , getFileName        :: FileName
  , getFileSizeBytes   :: Integer
  , getFileType        :: String
  , getFilePath        :: FilePath
  , getFilePermissions :: Permissions
  , getFileModifTime   :: FileSystemTime
  } deriving (Show, Eq)

-- | Type to represent real folder in virtual file system.
data Directory = Directory
  { getDirContent     :: [FileSystemElement]
  , getDirName        :: DirName
  , getDirPath        :: FilePath
  , getDirPermissions :: Permissions
  } deriving (Show, Eq)

type FileSystemElement = Either Directory File

-- | This type exists only because we can't get current time
-- when creating/modifying files inside virtual file system.
data FileSystemTime
  = UndefinedTime
  | Time UTCTime
  deriving Eq

instance Show FileSystemTime where
  show :: FileSystemTime -> String
  show t = case t of
    UndefinedTime -> "Undefined time"
    Time utc -> show utc

-- | Custom exception to represent possible errors in
-- virtual file system.
data FileManagerException 
  = InvalidPath String
  | InvalidName String
  | EntityNotFound String
  | UnknownEntity String
  | UnsupportedOperation String
  deriving (Eq, Show, Typeable)

instance Exception FileManagerException

-- | Returns formatted string which contains all relevant info
-- about file (name, path, size, permissions, type and last 
-- modification time) or folder (name, path, size, number of files
-- within it and permissions).
getFSEInfo :: FileSystemElement -> String
getFSEInfo (Right f) = "Information about file"
  ++ "\nName: " ++ getFileName f
  ++ "\nPath: " ++ getFilePath f
  ++ "\nSize: " ++ show (getFileSizeBytes f)
  ++ "\nType: " ++ getFileType f
  ++ "\n" ++ show (getFilePermissions f)
  ++ "\nLast modification time: " ++ show (getFileModifTime f)
  ++ "\n"
getFSEInfo (Left d) = "Information about directory"
  ++ "\nName: " ++ getDirName d
  ++ "\nPath: " ++ getDirPath d
  ++ "\nSize: " ++ show sz
  ++ "\nNumber of files: " ++ show num
  ++ "\n" ++ show (getDirPermissions d)
  ++ "\n"
    where
      (sz, num) = getDirSizeAndNumberOfFiles d

getDirSizeAndNumberOfFiles :: Directory -> (Integer, Integer)
getDirSizeAndNumberOfFiles (Directory {getDirContent = content}) = helper content (0, 0)
  where
    helper :: [FileSystemElement] -> (Integer, Integer) -> (Integer, Integer)
    helper [] p = p
    helper (x:xs) (sz, num) = case x of
      Right file -> helper xs (sz + getFileSizeBytes file, num + 1)
      Left dir   -> helper xs (sz + nSz, num + nNum)
        where
          (nSz, nNum) = getDirSizeAndNumberOfFiles dir