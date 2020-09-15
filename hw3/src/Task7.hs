module Task7
  ( changeExtension
  , deleteDirIfEmpty
  , getAllFileNames
  ) where

import Task5
  ( FS (..)
  , contents
  , fileNameTraversal
  , isDir
  )
import Data.List 
  ( foldl'
  )
import Lens.Micro
  ( (^..)
  , (%~)
  , (&)
  , filtered
  , traversed
  , 
  )
import System.FilePath
  ( replaceExtension
  )

-- | Changes extension for all files in given directory (non-recursive).
changeExtension :: FilePath -> FS -> FS
changeExtension newExt fs = 
  fs & contents . traversed . fileNameTraversal %~ (\file -> replaceExtension file newExt)

-- | Returns names of all files in given file system (recursive).
getAllFileNames :: FS -> [FilePath]
getAllFileNames fs = curFiles ++ (foldl' (\names dir -> names ++ getAllFileNames dir) [] curDirs)
  where
    curFiles = (fs ^.. contents . traversed . fileNameTraversal)
    curDirs = (fs ^.. contents . traversed . filtered isDir)

-- | Deletes specified directory if it is empty.
deleteDirIfEmpty :: FilePath -> FS -> FS
deleteDirIfEmpty nm fs = fs & contents %~ (filter isFileOrNotEmpty)
  where
    isFileOrNotEmpty :: FS -> Bool
    isFileOrNotEmpty (File _) = True
    isFileOrNotEmpty (Dir dirNm []) = dirNm /= nm
    isFileOrNotEmpty (Dir _ (_:_)) = True
