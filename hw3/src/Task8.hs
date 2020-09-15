{-# LANGUAGE InstanceSigs #-}

module Task8
  ( Config (..)
  , Grid (..)
  , Person (..)
  , Status (..)
  
  , emptyGrid
  , gridWithOneInfected
  , getPrintableGrid
  , makeStep
  ) where

import Control.Comonad 
  ( Comonad (..)
  )
import System.Random
  ( StdGen
  , next
  , random
  , split
  )
import Data.List 
  ( intercalate
  )

---------------------------------------------------------------------------------------------------

data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap :: (a -> b) -> ListZipper a -> ListZipper b
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight

---------------------------------------------------------------------------------------------------

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Functor Grid where
  fmap :: (a -> b) -> Grid a -> Grid b
  fmap f (Grid g) = Grid (fmap (fmap f) g)

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridRead

  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical

---------------------------------------------------------------------------------------------------

-- | Data type to represent person's health status.
data Status 
  = Healthy
  | StealthySick
  | Sick
  | Immune 
  deriving (Eq)

-- | Data type to represent person.
data Person
  = Person
  { status :: Status
  , daysRemaining :: Int
  , generator :: StdGen
  }

-- | Data type for simulation configuration.
data Config 
  = Config 
  { probability :: Double
  , stealthySickPeriod :: Int
  , sickPeriod :: Int
  , immunePeriod :: Int
  }

instance Show Person where
  show :: Person -> String
  show (Person st _ _) = case st of
    Healthy -> "_"
    StealthySick -> "!"
    Sick -> "#"
    Immune -> "@"

nextStatus :: Status -> Status
nextStatus st = case st of
  Healthy -> StealthySick
  StealthySick -> Sick
  Sick -> Immune
  Immune -> Healthy

nextGenerator :: StdGen -> StdGen
nextGenerator gen' = snd $ next gen'

---------------------------------------------------------------------------------------------------

listLeft :: ListZipper a -> ListZipper a
listLeft (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _ = error "listLeft"

listRight :: ListZipper a -> ListZipper a
listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _ = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (a -> a) -> (a -> a) -> a -> ListZipper a
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

---------------------------------------------------------------------------------------------------

up :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)

down :: Grid a -> Grid a
down (Grid g) = Grid (listRight g)

left :: Grid a -> Grid a
left (Grid g) = Grid (fmap listLeft g)

right :: Grid a -> Grid a
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right

vertical :: Grid a -> ListZipper (Grid a)
vertical = genericMove up down

---------------------------------------------------------------------------------------------------

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals
  where 
    horizontals = [left, right]
    verticals   = [up, down]

-- | Function to change person's health status.
handleStatusChange :: Config -> Person -> Person
handleStatusChange (Config _ _ sp ip) (Person stat cnt gen) = helper nCntByStatus
  where
    helper :: Int -> Person
    helper nCnt =
      if cnt == 0
      then Person (nextStatus stat) nCnt gen
      else Person stat (cnt - 1) gen

    nCntByStatus :: Int
    nCntByStatus = case stat of
      StealthySick -> sp
      Sick -> ip
      _ -> 1

-- | Function to emulate the process of infection: getting all neighbours, counting infected
-- and then performing 'k' attempts to infect current cell, where 'k' is number of infected.
tryToInfect :: Double -> Grid Person -> (StdGen, Bool)
tryToInfect p g = checkProb (infectedCnt $ map (\dir -> status $ extract $ dir g) neighbours) gen
  where
    (Person _ _ gen) = gridRead g

    checkProb :: Int -> StdGen -> (StdGen, Bool)
    checkProb times gen'
      | times == 0 = (gen', False)
      | otherwise = 
        let
          (prob, nextGen') = random gen'
        in
          if prob <= p
          then (nextGen', True)
          else checkProb (times - 1) nextGen'

    infectedCnt :: [Status] -> Int
    infectedCnt = length . filter sickPredicate

    sickPredicate :: Status -> Bool
    sickPredicate st = st == StealthySick || st == Sick

-- | Function to represent rules of simulation.
rule :: Config -> Grid Person -> Person
rule cfg@(Config p stp _ _) g = case stat of
  Healthy -> case tryToInfect p g of
    (gen', True) -> Person StealthySick stp gen'
    (gen', False) -> Person stat d gen'
  _ -> handleStatusChange cfg (gridRead g)
  where
    (Person stat d _) = gridRead g

---------------------------------------------------------------------------------------------------

-- | Returns empty grid.
emptyGrid :: StdGen -> Grid Person
emptyGrid gen = Grid $ genericMove (nextZipper fst) (nextZipper snd) (createRow gen)
  where
    nextElem :: ((StdGen, StdGen) -> StdGen) -> (Person -> Person)
    nextElem f = \(Person st d gen') -> Person st d (f $ split gen')

    nextZipper :: ((StdGen, StdGen) -> StdGen) -> (ListZipper Person -> ListZipper Person)
    nextZipper f = \(LZ _ (Person _ _ gen') _) -> createRow $ (f $ split $ nextGenerator gen')

    createRow :: StdGen -> ListZipper Person
    createRow gen' = genericMove (nextElem fst) (nextElem snd) (Person Healthy 1 gen')

-- | Returns grid with one stealthy infected person.
gridWithOneInfected :: Config -> StdGen -> Grid Person
gridWithOneInfected (Config _ stp _ _) gen = gridWrite (Person StealthySick stp gen) (emptyGrid gen)

-- | Performs one step of simulation.
makeStep :: Config -> Grid Person -> Grid Person
makeStep cfg = extend $ rule cfg

-- | Returns string represenation of given grid, where:
-- healthy is '_'
-- stealthy sick is '!'
-- sick with symptoms is '#'
-- person with immunity is '@'
getPrintableGrid :: Int -> Grid Person -> String
getPrintableGrid radius (Grid g) = 
  intercalate "\n" (map (\lz -> concatMap show (toList lz radius)) (toList g radius))