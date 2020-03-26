{-# LANGUAGE InstanceSigs #-}

module Block1_Task2 
  ( Nat (..)

  , add
  , divNat
  , modNat
  , mul
  , sub

  , fromNat
  , toNat

  , compareNats
  , isEqual
  , isEven
  ) where

data Nat = Z | S Nat
  deriving Show

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) (S x) (S y) = x == y
  (==) Z Z         = True
  (==) Z _         = False
  (==) _ Z         = False

instance Ord Nat where
  compare :: Nat -> Nat -> Ordering
  compare (S x) (S y) = compare x y
  compare Z Z         = EQ
  compare Z _         = LT
  compare _ Z         = GT

fromNat :: Nat -> Integer
fromNat Z     = 0
fromNat (S n) = fromNat n + 1

toNat :: Integer -> Nat
toNat x
  | x < 0     = error "Natural number cannot be less than zero"
  | x == 0    = Z
  | otherwise = S $ toNat (x - 1)

add :: Nat -> Nat -> Nat
add x (S y) = S (add x y)
add x Z     = x

mul :: Nat -> Nat -> Nat
mul x (S y) = add x (mul x y)
mul _ Z     = Z

sub :: Nat -> Nat -> Nat
sub (S x) (S y) = sub x y
sub Z _         = Z
sub x Z         = x

divNat :: Nat -> Nat -> Nat
divNat _ Z = error "Division by zero"
divNat Z _ = Z
divNat x y = helper x y Z
  where
    helper :: Nat -> Nat -> Nat -> Nat
    helper Z _ res = res
    helper f s res =
      if f < s
      then res
      else helper (sub f s) s (S res)

modNat :: Nat -> Nat -> Nat
modNat _ Z = error "Modulo by zero"
modNat Z _ = Z
modNat x y =
  if x < y
  then x
  else modNat (sub x y) y

isEqual :: Nat -> Nat -> Bool
isEqual x y = x == y

compareNats :: Nat -> Nat -> Ordering
compareNats x y = compare x y

isEven :: Nat -> Bool
isEven x = helper x 0
  where
    helper :: Nat -> Integer -> Bool
    helper (S y) cnt = helper y (cnt + 1)
    helper Z cnt     = cnt `mod` 2 == 0
