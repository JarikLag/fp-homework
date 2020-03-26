module Block4_Task1 where

import Text.Read (readMaybe)

stringSum :: String -> Maybe Int
stringSum s = getSum $ traverse readMaybe (words s)
  where
    getSum :: Maybe [Int] -> Maybe Int
    getSum Nothing   = Nothing
    getSum (Just xs) = Just $ sum xs