module Main 
  ( main
  ) where

import Test.Hspec (hspec)

import Block1_Task1Spec (fullSpec)
import Block1_Task2Spec (fullSpec)
import Block1_Task3AndBlock2_Task1Spec (fullSpec)
import Block2_Task2Spec (fullSpec)
import Block3_Task1Spec (fullSpec)
import Block3_Task2Spec (fullSpec)
import Block4_Task1Spec (fullSpec)
import Block5_Task1Spec (fullSpec)
import Block5_Task2Spec (fullSpec)
import Block6Spec (fullSpec)

main :: IO ()
main = hspec $ do
  Block1_Task1Spec.fullSpec
  Block1_Task2Spec.fullSpec
  Block1_Task3AndBlock2_Task1Spec.fullSpec
  Block2_Task2Spec.fullSpec
  Block3_Task1Spec.fullSpec
  Block3_Task2Spec.fullSpec
  Block4_Task1Spec.fullSpec
  Block5_Task1Spec.fullSpec
  Block5_Task2Spec.fullSpec
  Block6Spec.fullSpec