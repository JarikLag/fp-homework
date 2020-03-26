module Block4_Task1 
  ( stringSum
  ) where

import Text.Read (readMaybe)

-- | Takes string and parses int numbers from it,
-- calculates their sum and return Just sum. If 
-- some part of string cannot be converted to int
-- returns Nothing.
stringSum :: String -> Maybe Int
stringSum s = getSum $ traverse readMaybe (words s)
  where
    getSum :: Maybe [Int] -> Maybe Int
    getSum Nothing   = Nothing
    getSum (Just xs) = Just $ sum xs