module Task1Bench
  ( areaBench
  , perimeterBench
  ) where

import Task1
  ( Point (..)
  , doubleArea
  , doubleAreaNaive
  , perimeter
  , perimeterNaive
  )
import System.Random
  ( StdGen
  , newStdGen  
  , random
  )
import Criterion
  ( Benchmark
  , bench
  , bgroup
  , env
  , nf
  )
import Criterion.Main
  ( defaultMain
  )
import Data.List
  ( unfoldr
  )
import Control.DeepSeq 
  ( NFData
  )

randomListPure :: Int -> StdGen -> [Int]
randomListPure n = take n . unfoldr (Just . random)

randomList :: Int -> IO [Int]
randomList n = do
  seed <- newStdGen
  return $ randomListPure n seed

randomPointsEnv :: Int -> IO [Point]
randomPointsEnv n = do
  xs <- randomList n
  ys <- randomList n
  return $ zipWith Point xs ys

pointsBench :: (Num a, NFData a) => ([Point] -> a) -> Int -> Benchmark
pointsBench op n = env (randomPointsEnv n) (\args -> bench (show n) (nf op args))

pointsBenches :: (Num a, NFData a) => [Int] -> ([Point] -> a) -> [Benchmark]
pointsBenches nums op = map (pointsBench op) nums 

numberOfPoints :: [Int]
numberOfPoints = [1000, 100000, 10000000]

perimeterBench :: IO ()
perimeterBench = 
  defaultMain [
    bgroup "perimeter" $ pointsBenches numberOfPoints perimeter
  , bgroup "perimeterNaive" $ pointsBenches numberOfPoints perimeterNaive
  ]

areaBench :: IO ()
areaBench = 
  defaultMain [
    bgroup "doubleArea" $ pointsBenches numberOfPoints doubleArea
  , bgroup "doubleAreaNaive" $ pointsBenches numberOfPoints doubleAreaNaive
  ]