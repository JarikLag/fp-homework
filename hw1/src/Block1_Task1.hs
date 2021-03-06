{-# LANGUAGE InstanceSigs #-}

module Block1_Task1 
  ( Day (..)

  , afterDays
  , daysToParty
  , isWeekend
  , nextDay 
  ) where

-- | Data type which represents days of week.
data Day 
  = Monday 
  | Tuesday 
  | Wednesday 
  | Thursday 
  | Friday 
  | Saturday 
  | Sunday
  deriving Show

instance Eq Day where
  (==) :: Day -> Day -> Bool
  (==) Monday Monday       = True
  (==) Monday _            = False
  (==) Tuesday Tuesday     = True
  (==) Tuesday _           = False
  (==) Wednesday Wednesday = True
  (==) Wednesday _         = False
  (==) Thursday Thursday   = True
  (==) Thursday _          = False
  (==) Friday Friday       = True
  (==) Friday _            = False
  (==) Saturday Saturday   = True
  (==) Saturday _          = False
  (==) Sunday Sunday       = True
  (==) Sunday _            = False

-- | Returns the next day. E.g. the next day after Monday
-- is Tuesday.
nextDay :: Day -> Day
nextDay day = case day of
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday

-- | Returns the day of the week that will occur after 
-- the specified number of days.
afterDays :: Day -> Integer -> Day
afterDays day n
  | n < 0     = error "Negative number of days"
  | n == 0    = day 
  | n > 7     = afterDays (nextDay day) ((n `mod` 7) - 1)
  | otherwise = afterDays (nextDay day) (n - 1)

-- | Checks whether the passed day is a weekend.
isWeekend :: Day -> Bool
isWeekend day = case day of
  Saturday -> True
  Sunday   -> True
  _        -> False

-- | Returns number of days remaining untils Friday.
daysToParty :: Day -> Integer
daysToParty day = helper day 0
  where
    helper :: Day -> Integer -> Integer
    helper curDay curNum = case curDay of
      Friday -> curNum
      _      -> helper (nextDay curDay) (curNum + 1)
