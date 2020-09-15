module Task5
  ( FS (..)
  , buildFileSystem
  , contents
  , dirNameTraversal
  , fileNameTraversal
  , name

  , addSuffixToFSRootName
  , changeFSRootNameToSlash
  , dirName
  , fileName
  , getFileNames
  , getFirstSubdirName
  , isDir
  , isFile
  , subdirs
  ) where

import System.Directory 
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath 
  ( (</>)
  , dropTrailingPathSeparator
  , isValid
  , splitPath
  )
import Lens.Micro
  ( (^.)
  , (^..)
  , (^?)
  , (.~)
  , (%~)
  , (&)
  , Lens'
  , Traversal'
  , _head
  , filtered
  , lens
  , traversed
  )

---------------------------------------------------------------------------------------------------

-- | Data type to represent file system.
data FS 
  = Dir 
  { _name     :: FilePath  -- название папки, не полный путь
  , _contents :: [FS]
  }
  | File
  { _name     :: FilePath  -- название файла, не полный путь
  } deriving (Show, Eq)

-- | Lens for name.
name :: Lens' FS FilePath
name = lens _name (\fs newName -> fs {_name = newName} )

-- | Traversal for content.
contents :: Traversal' FS [FS]
contents f fs@Dir {_contents = cnt} = (\newContents -> fs {_contents = newContents}) <$> f cnt
contents _ fs = pure fs

-- | Traversal for file name.
fileNameTraversal :: Traversal' FS FilePath
fileNameTraversal f (File x) = File <$> f x
fileNameTraversal _ fs       = pure fs

-- | Traversal for directory name.
dirNameTraversal :: Traversal' FS FilePath
dirNameTraversal f fs@(Dir {_name = nm}) = (\newName -> fs {_name = newName}) <$> f nm
dirNameTraversal _ fs = pure fs

---------------------------------------------------------------------------------------------------

-- | Checks is FS object Dir or not.
isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir _         = False

-- | Checks is FS object File or not.
isFile :: FS -> Bool
isFile (File _) = True
isFile _        = False

-- | Returns list of subdirectories for given FS.
subdirs :: FS -> [FS]
subdirs fs = fs ^.. contents . traversed . (filtered isDir)

-- | Returns Dir name or Nothing otherwise.
dirName :: FS -> Maybe FilePath
dirName fs@(Dir _ _) = Just $ fs ^. name
dirName _            = Nothing

-- | Returns File name ot empty string otherwise.
fileName :: FS -> FilePath
fileName fs@(File _) = fs ^. name
fileName _           = ""

-- | Changes name of file system root to slash.
changeFSRootNameToSlash :: FS -> FS
changeFSRootNameToSlash fs = fs & name .~ "/"

-- | Adds specified suffix to name of file system root.
addSuffixToFSRootName :: FS -> FilePath -> FS
addSuffixToFSRootName fs suff = fs & name %~ (\nm -> nm ++ suff)

-- | Returns name of first subdirectory.
getFirstSubdirName :: FS -> Maybe FilePath
getFirstSubdirName fs = (subdirs fs) ^? _head . name

-- | Returns names of all files in directory (non-recursive).
getFileNames :: FS -> [FilePath]
getFileNames fs = fs ^.. contents . traversed . (filtered isFile) . name

---------------------------------------------------------------------------------------------------

-- | Builds virtual file system by given path. If path is not valid returns Nothing.
-- Can throw exceptions.
buildFileSystem :: FilePath -> IO (Maybe FS)
buildFileSystem rootPath = do
  case getNameByPath rootPath of
    Nothing -> return Nothing
    Just elemName -> do
      isElemFile <- isRealFile rootPath
      if isElemFile
      then return $ Just $ File elemName
      else do
        isElemDir <- isRealDir rootPath
        if isElemDir
        then do
          dirContentRaw <- listDirectory rootPath
          dirContent <- fillDirWithContent rootPath dirContentRaw
          return $ Just $ Dir elemName dirContent
        else return Nothing

-- | Takes path to root directory and content of root directory  
-- as list of relative paths and trying to build FS for each element.
fillDirWithContent :: FilePath -> [FilePath] -> IO [FS]
fillDirWithContent rootPath paths = fillDir paths []
  where
    fillDir :: [FilePath] -> [FS] -> IO [FS]
    fillDir [] res = return res
    fillDir (c:cs) res = do
      maybeFS <- buildFileSystem (rootPath </> c)
      case maybeFS of
        Nothing -> fillDir cs res
        Just fs -> fillDir cs (fs:res)

-- | Extracts folder/file name from path to it.
getNameByPath :: FilePath -> Maybe FilePath
getNameByPath fp
  | isValidPath fp = Just $ last $ splitPathAndDropTrailSep fp
  | otherwise      = Nothing

-- | Checks is given path leads to a file.
isRealFile :: FilePath -> IO Bool
isRealFile = doesFileExist

-- | Checks is given path leads to a folder.
isRealDir :: FilePath -> IO Bool
isRealDir = doesDirectoryExist

-- | Validate path.
isValidPath :: FilePath -> Bool
isValidPath = isValid

-- | Splits path and removes trailing separators.
splitPathAndDropTrailSep :: FilePath -> [FilePath]
splitPathAndDropTrailSep fp = map dropTrailingPathSeparator (splitPath fp)