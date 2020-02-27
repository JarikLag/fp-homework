module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg x f = f x

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = helper first second
  where
    helper
      :: (Neg (Either a (Neg a)) -> Neg a)
      -> (Neg (Either a (Neg a)) -> Neg (Neg a))
      -> Neg (Neg (Either a (Neg a)))
    helper f g x = g x $ f x
    first :: Neg (Either a (Neg a)) -> Neg a
    first = contrPos Left
    second :: Neg (Either a (Neg a)) -> Neg (Neg a)
    second = contrPos Right
    contrPos :: (a -> b) -> (Neg b -> Neg a)
    contrPos f g x = g $ f x

pierce :: ((a -> b) -> a) -> a
pierce = undefined --doesn't have proof

doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined --doesn't have proof

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = helper doubleNeg
  where
    helper :: (a -> b) -> (Neg b -> Neg a)
    helper f g x = g $ f x