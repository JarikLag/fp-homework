module Block5_Task2 
  ( moving
  ) where

import Data.List (genericLength)
import Control.Monad.State (State, evalState, get, put)

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

initState :: ([Double], [Int])
initState = ([], [])

moving :: Int -> [Int] -> [Double]
moving n list 
  | n <= 0    = error "Window size cannot be less than zero"
  | otherwise = reverse $ evalState (helper n list) initState
  where
    helper :: Int -> [Int] -> State ([Double], [Int]) [Double]
    helper _ [] = do
      (result, _) <- get
      return result
    helper m (x:xs) = do
      (result, queue) <- get
      let 
        nQueue = 
          if length queue < m
          then x : queue
          else x : init queue
        avg = average nQueue
        nResult = avg : result
      put (nResult, nQueue)
      helper m xs