{-# LANGUAGE Rank2Types #-}

module Task6 
  ( cd
  , file
  , ls
  ) where

import Task5
  ( FS
  , contents
  , fileNameTraversal
  , isDir
  , name
  )
import Lens.Micro
  ( (^.)
  , Traversal'
  , failing
  , filtered
  , traversed
  )

-- | Moves from current directory to specified. 
cd :: FilePath -> Traversal' FS FS
cd nm = contents . traversed . (filtered dirFilter)
  where
    dirFilter :: FS -> Bool
    dirFilter fs
      | isDir fs = (fs ^. name) == nm
      | otherwise = False

-- | Returns list of names of all files and directories in
-- current directory.
ls :: Traversal' FS FilePath
ls = contents . traversed . name

-- | Returns name of specified file if it is present
-- in current directory.
file :: FilePath -> Traversal' FS FilePath
file nm = failing 
  (fileNameTraversal . filtered (== nm)) 
  (contents . traversed . fileNameTraversal . filtered (== nm))    