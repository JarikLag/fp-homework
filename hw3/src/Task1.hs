{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}

module Task1
  ( Point (..)
  , crossProduct
  , minus
  , plus
  , scalarProduct

  , doubleArea
  , doubleAreaNaive
  , perimeter
  , perimeterNaive
  ) where

import Control.DeepSeq 
  ( NFData
  , rnf
  )

-- | Data type to represent point with 2 coordinates.
data Point 
  = Point
  { x :: !Int
  , y :: !Int
  } deriving (Show)

instance NFData Point where
  rnf :: Point -> ()
  rnf (Point !ax !ay) = ax `seq` (ay `seq` ())

-- | Calculates sum of two points.
plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | Calculates difference of two points.
minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- | Calculates scalar product of two points.
scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

-- | Calculates pseudo-product of two points.
crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- | Calculates distance between two points.
distance :: Point -> Point -> Double
distance p1 p2 = let nP = p1 `minus` p2 in 
  sqrt $ fromIntegral $ scalarProduct nP nP

-- | Calculates perimeter of given polygon fast.
perimeter :: [Point] -> Double
perimeter [] = 0.0
perimeter points@(fstPoint:_) = perimeterHelper fstPoint points 0.0
  where
    perimeterHelper :: Point -> [Point] -> Double -> Double
    perimeterHelper _ [] !acc = acc
    perimeterHelper start [end] !acc = perimeterHelper start [] (acc + distance start end)
    perimeterHelper start (p1:p2:ps) !acc = perimeterHelper start (p2:ps) (acc + distance p1 p2)

-- | Calculates doubled area of given polygon fast.
doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea points@(fstPoint:_) = abs $ doubleAreaHelper fstPoint points 0
  where
    doubleAreaHelper :: Point -> [Point] -> Int -> Int
    doubleAreaHelper _ [] !acc = acc
    doubleAreaHelper start [end] !acc = doubleAreaHelper start [] (acc + crossProduct end start)
    doubleAreaHelper start (p1:p2:ps) !acc = doubleAreaHelper start (p2:ps) (acc + crossProduct p1 p2)

-- | Calculates perimeter of given polygon slow.
perimeterNaive :: [Point] -> Double
perimeterNaive [] = 0.0
perimeterNaive list = sum $ zipWith distance list (tail $ cycle list)

-- | Calculates doubled area of given polygon slow.
doubleAreaNaive :: [Point] -> Int
doubleAreaNaive [] = 0
doubleAreaNaive list = abs $ sum $ zipWith crossProduct list (tail $ cycle list)