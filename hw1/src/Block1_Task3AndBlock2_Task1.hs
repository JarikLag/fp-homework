{-# LANGUAGE InstanceSigs #-}

module Block1_Task3AndBlock2_Task1
  ( Tree (..)

  , emptyTree
  , fromList
  , toList

  , deleteElement
  , findElement
  , getTreeSize
  , insertElement
  , isTreeEmpty
  ) where

import Data.List (foldl')

data Tree a = Leaf | Branch [a] (Tree a) (Tree a)
  deriving Show

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Branch list l r) = foldMap f l `mappend` foldMap f list `mappend` foldMap f r

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z (Branch list l r) = foldr f (foldr f (foldr f z r) list) l

instance Ord a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) Leaf Leaf = True
  (==) Leaf _    = False
  (==) _    Leaf = False
  (==) (Branch list1 l1 r1) (Branch list2 l2 r2) = list1 == list2 && l1 == l2 && r1 == r2

emptyTree :: Ord a => Tree a
emptyTree = Leaf

fromList :: Ord a => [a] -> Tree a
fromList xs = foldl' insertElement emptyTree xs

toList :: Ord a => Tree a -> [a]
toList = foldMap (\x -> [x])

isTreeEmpty :: Ord a => Tree a -> Bool
isTreeEmpty Leaf = True
isTreeEmpty _    = False

getTreeSize :: Ord a => Tree a -> Int
getTreeSize Leaf             = 0
getTreeSize (Branch val l r) = (length val) + getTreeSize l + getTreeSize r

findElement :: Ord a => Tree a -> a -> Maybe a
findElement Leaf _ = Nothing
findElement (Branch list l r) x = 
  let elemnt = head list in
    if elemnt == x
    then Just x
    else
      if elemnt > x
      then findElement l x
      else findElement r x

insertElement :: Ord a => Tree a -> a -> Tree a
insertElement Leaf x = Branch [x] Leaf Leaf
insertElement (Branch list l r) x =
  let elemnt = head list in
    if elemnt == x
    then Branch (x:list) l r
    else
      if elemnt > x
      then Branch list (insertElement l x) r
      else Branch list l (insertElement r x)

deleteElement :: Ord a => Tree a -> a -> Tree a
deleteElement Leaf              _ = Leaf
deleteElement (Branch list l r) x =
  let elemnt = head list in
    if elemnt == x
    then
      if length list > 1
      then Branch (tail list) l r
      else
        if l == Leaf && r == Leaf
        then Leaf
        else
          if l == Leaf || r == Leaf
          then
            if l == Leaf
            then r
            else l
          else
            let 
              minEl = getMinimumElement r
              (Branch extList _ extR) = extractBranch r minEl
              del = replaceBranch r minEl extR
            in
              Branch extList l del
    else
      if elemnt > x
      then Branch list (deleteElement l x) r
      else Branch list l (deleteElement r x)

getMinimumElement :: Ord a => Tree a -> a
getMinimumElement Leaf = error "No minimum element in empty tree"
getMinimumElement (Branch list Leaf _) = head list
getMinimumElement (Branch _ l _)       = getMinimumElement l

replaceBranch :: Ord a => Tree a -> a -> Tree a -> Tree a
replaceBranch Leaf _ _ = Leaf
replaceBranch (Branch list l r) x repl = 
  let elemnt = head list in
    if elemnt == x
    then repl
    else
      if elemnt > x
      then Branch list (replaceBranch l x repl) r
      else Branch list l (replaceBranch r x repl)

extractBranch :: Ord a => Tree a -> a -> Tree a
extractBranch Leaf _ = Leaf
extractBranch (Branch list l r) x = 
  let elemnt = head list in
    if elemnt == x
    then (Branch list l r)
    else
      if elemnt > x
      then extractBranch l x
      else extractBranch r x