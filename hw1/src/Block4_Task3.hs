{-# LANGUAGE InstanceSigs #-}

module Block4_Task3 
  ( NonEmpty (..)
  ) where

data NonEmpty a = a :| [a]
  deriving Show

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x:|xs) = f x <> foldMap f xs

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x:|xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f:|fs) (x:|xs) = f x :| (fmap f xs ++ (fs <*> (x:xs)))

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x:|xs) = fmap (:|) (f x) <*> traverse f xs