module Task4 
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix 
  ) where

import Data.Function

iterateElement :: a -> [a]
iterateElement x = fix (\rec -> x : rec)

fibonacci :: Integer -> Integer
fibonacci n
  | n >= 1 = fix (\rec k -> if k == 1 || k == 0
    then k
    else rec (k - 1) + rec (k - 2)) n
  | n <= -1 = fix (\rec k -> if k == -1 || k == 0
    then k * (-1)
    else rec (k + 2) - rec (k + 1)) n
  | otherwise = 0

factorial :: Integer -> Integer
factorial n = fix (\rec k -> if k == 0 then 1 else k * rec (k - 1)) n

mapFix :: (a -> b) -> [a] -> [b]
mapFix f xs = fix (\rec list -> if null list 
  then [] 
  else f (head list) : rec (tail list)) xs