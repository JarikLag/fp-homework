{-# LANGUAGE ScopedTypeVariables #-}

module Task7
  ( termOne
  , termSecond
  , termThird
  ) where

import Data.Either (lefts, rights)

termOne :: Bool
termOne = nullDotHead `dollar` myResult
  where
    dollar :: (a -> b) -> a -> b
    dollar = ($)
    nullDotHead :: [[Char]] -> Bool
    nullDotHead = myNull `dot` myHead
      where
        myNull :: [a] -> Bool
        myNull = null
        dot :: (b -> c) -> (a -> b) -> a -> c
        dot = (.)
        myHead :: [a] -> a
        myHead = head
    myResult :: [[Char]]
    myResult = myMap myFunc myList
      where
        myMap :: (a -> b) -> [a] -> [b]
        myMap = map
        myFunc :: (a -> b, a) -> b
        myFunc = (myUncurry myId)
          where
            myUncurry :: (a -> b -> c) -> (a, b) -> c
            myUncurry = uncurry
            myId :: a -> a
            myId = id
        myList :: [([Char] -> [Char], [Char])]
        myList = [myPair]
          where
            myPair :: ([Char] -> [Char], [Char])
            myPair = (myFirst, mySecond)
              where
                myFirst :: [Char] -> [Char]
                myFirst = myConcat myString
                  where
                    myConcat :: [Char] -> [Char] -> [Char]
                    myConcat = (++)
                    myString :: [Char]
                    myString = "Dorian "
                mySecond :: [Char]
                mySecond = " Grey"

termSecond :: (Num a, Num b) => [(a, b)]
termSecond = myLambda myList
  where
    myLambda :: forall a b. [Either a b] -> [(a, b)]
    myLambda = (\x -> myZip (myLefts x :: [a]) (myRights x :: [b]))
      where
        myZip :: [a] -> [b] -> [(a, b)]
        myZip = zip
        myLefts :: [Either a b] -> [a]
        myLefts = lefts
        myRights :: [Either a b] -> [b]
        myRights = rights
    myList :: (Num a, Num b) => [Either a b]
    myList = [myFirst, mySecond]
      where
        myFirst :: Num a => Either a b
        myFirst = myLeft (expressionForLeft)
          where
            myLeft :: a -> Either a b
            myLeft = Left
            expressionForLeft :: Num a => a
            expressionForLeft = firstPlusArg `myPlus` secondPlusArg
              where
                firstPlusArg :: Num a => a
                firstPlusArg = 1
                myPlus :: Num a => a -> a -> a
                myPlus = (+)
                secondPlusArg :: Num a => a
                secondPlusArg = 2
        mySecond :: Num b => Either a b
        mySecond = myRight (expressionForRight)
          where
            myRight :: b -> Either a b
            myRight = Right
            expressionForRight :: Num a => a
            expressionForRight = firstPowArg `myPow` secondPowArg
              where
                firstPowArg :: Num a => a
                firstPowArg = 2
                myPow :: (Num a, Integral b) => a -> b -> a
                myPow = (^)
                secondPowArg :: Integer
                secondPowArg = 6

termThird :: Integral a => a -> Bool
termThird = 
  let
    myImpl :: Bool -> Bool -> Bool
    myImpl = \x y -> (myNot x :: Bool) `myOr` y
      where
        myNot :: Bool -> Bool
        myNot = not
        myOr :: Bool -> Bool -> Bool
        myOr = (||)
  in
    let
      myIsMod2 :: forall a. Integral a => a -> Bool
      myIsMod2 = \x -> (x `myMod` myTwo :: Integral a => a) `myEqual` myZero
        where
          myMod :: Integral a => a -> a -> a
          myMod = mod
          myTwo :: Integral a => a
          myTwo = 2
          myEqual :: Eq a => a -> a -> Bool
          myEqual = (==)
          myZero :: Integral a => a
          myZero = 0
    in
      let
        myIsMod4 :: forall a. Integral a => a -> Bool
        myIsMod4 = \x -> (x `myMod` myFour :: Integral a => a) `myEqual` myZero
          where
            myMod :: Integral a => a -> a -> a
            myMod = mod
            myFour :: Integral a => a
            myFour = 4
            myEqual :: Eq a => a -> a -> Bool
            myEqual = (==)
            myZero :: Integral a => a
            myZero = 0
      in
        \x -> (myIsMod4 x :: Bool) `myImpl` (myIsMod2 x :: Bool)