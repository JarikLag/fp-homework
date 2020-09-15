module Main 
  ( main
  ) where

import Test.Hspec (hspec)

import Task2Spec (fullSpec)
import Task3Spec (fullSpec)
import Task6Spec (fullSpec)
import Task7Spec (fullSpec)

main :: IO ()
main = hspec $ do
  Task2Spec.fullSpec
  Task3Spec.fullSpec
  Task6Spec.fullSpec
  Task7Spec.fullSpec