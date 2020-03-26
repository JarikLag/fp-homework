{-# LANGUAGE InstanceSigs #-}

module Block3_Task2
  ( Endo (..)
  , Name (..)
  , NonEmpty (..)
  , ThisOrThat (..)
  ) where

-- | Data type which represents non-empty list.
data NonEmpty a 
  = a :| [a]
  deriving Show

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x:|xs) (y:|ys) = x :| (xs ++ (y:ys))

-----------------------------------------------------------------------

-- | Data type which can contains A or B or A and B.
data ThisOrThat a b 
  = This a 
  | That b 
  | Both a b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b 
  (<>) (This a)     (This b)     = This (a <> b)
  (<>) (This a)     (That b)     = Both a b
  (<>) (This a)     (Both b c)   = Both (a <> b) c
  (<>) (That a)     (This b)     = Both b a
  (<>) (That a)     (That b)     = That (a <> b)
  (<>) (That b)     (Both a c)   = Both a (b <> c)
  (<>) (Both b c)   (This a)     = Both (b <> a) c
  (<>) (Both a c)   (That b)     = Both a (c <> b)
  (<>) (Both a1 b1) (Both a2 b2) = Both (a1 <> a2) (b1 <> b2)

-----------------------------------------------------------------------

-- | Wrapper for strings which are concatenated with dot symbol.
newtype Name 
  = Name String
  deriving Show

instance Monoid Name where
  mempty :: Name
  mempty = Name ""

instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  (<>) (Name "") (Name b) = Name b
  (<>) (Name a) (Name "") = Name a
  (<>) (Name a) (Name b)  = Name (a ++ "." ++ b)

-----------------------------------------------------------------------

-- | Wrapper for functions like a -> a.
newtype Endo a = Endo { getEndo :: a -> a }

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo id

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) (Endo {getEndo = f}) (Endo {getEndo = g}) = Endo (f . g)  