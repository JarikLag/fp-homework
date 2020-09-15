{-# LANGUAGE TypeApplications #-}

module Main where

import System.Random
  ( newStdGen
  )
import Task8
  ( Config (..)
  , Grid (..)
  , Person
  , getPrintableGrid
  , gridWithOneInfected
  , makeStep
  )

main :: IO ()
main = do
  gen <- newStdGen
  putStrLn "Welcome to COVID-19 simulation."
  putStrLn "To start we need initial configuration."
  putStrLn "Enter probability:"
  prob <- getDouble
  putStrLn "Enter incubatory period:"
  stealthy <- getInt
  putStrLn "Enter sickness period:"
  sick <- getInt
  putStrLn "Enter immunity period:"
  immune <- getInt
  let cfg = Config prob stealthy sick immune
  let grid = gridWithOneInfected cfg gen
  putStrLn "Enter grid radius to print:"
  radius <- getInt
  putStrLn "Enter number of days you want to simulate:"
  days <- getInt
  goGrid days radius cfg grid 

getDouble :: IO Double
getDouble = do
  line <- getLine
  return $ read @Double line

getInt :: IO Int
getInt = do
  line <- getLine
  return $ read @Int line

goGrid :: Int -> Int -> Config -> Grid Person -> IO ()
goGrid 0 rad cfg grid = do
  putStrLn "Do you want to see more steps? y/n"
  answer <- getLine
  if answer == "y"
  then do
    putStrLn "Enter number of days you want to simulate:"
    days <- getInt
    goGrid days rad cfg grid
  else
    if answer == "n"
    then return ()
    else do
      putStrLn "Invalid input. Try again."
      goGrid 0 rad cfg grid
goGrid x rad cfg grid = do
  putStrLn $ getPrintableGrid rad grid
  putStrLn ""
  goGrid (x - 1) rad cfg (makeStep cfg grid)
