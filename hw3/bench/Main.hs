module Main where

import Task1Bench
  ( areaBench
  , perimeterBench
  )

main :: IO ()
main = do
  perimeterBench
  areaBench