module Block2_Task2
  ( joinWith
  , splitOn
  ) where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))

-- | Splits the list into sublists by element and returns
-- NonEmpty of lists.
-- >>> splitOn '/' "path/to/file"
-- "path" :| ["to","file"]
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn delim list = foldr (helper delim) ([]:|[]) list
  where
    helper :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
    helper delimC c (x:|xs) = 
      if c == delimC
      then [] :| (x:xs)
      else (c:x) :| xs

-- | Join given NonEmpty of lists into one list with element.
-- >>> joinWith '/' ("path" :| ["to", "file"])
-- "path/to/file"
joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (x:|[]) = x
joinWith delim (x:|xs) = foldl' (\y z -> y ++ [delim] ++ z) x xs
