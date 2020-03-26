{-# LANGUAGE InstanceSigs #-}

module Block4_Task2 
  ( Tree (..)
  ) where

data Tree a = Branch (Tree a) (Tree a) | Leaf a
  deriving Show

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf x)     = f x
  foldMap f (Branch x y) = foldMap f x <> foldMap f y

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x)     = Leaf $ f x
  fmap f (Branch x y) = Branch (fmap f x) (fmap f y)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf f)     x = fmap f x
  (<*>) (Branch f g) x = Branch (f <*> x) (g <*> x)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x)     = fmap (Leaf) (f x)
  traverse f (Branch x y) = (fmap (Branch) (traverse f x)) <*> traverse f y