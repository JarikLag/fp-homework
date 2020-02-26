module Task6 
  ( firstTerm
  , firstTermWHNF
  , foo
  , secondTerm
  , secondTermWHNF
  ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

firstTerm :: (Either String a, Either String b)
firstTerm = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

firstTermWHNF :: (Either String a, Either String b)
firstTermWHNF = 
  ( Left ("harold" ++ " hide " ++ "the " ++ "pain")
  , Left ("harold" ++ " hide " ++ "the " ++ "pain")
  )

foo :: Char -> Maybe Double
foo char =
    case char == 'o' of
      True -> Just $ exp pi
      False -> Nothing

secondTerm :: Bool
secondTerm = null $ mapMaybe foo "pole chudes ochen' chudesno"

secondTermWHNF :: Bool
secondTermWHNF = False