{-# LANGUAGE InstanceSigs #-}

module Block6
  ( Parser (..)

  , element
  , eof
  , ok
  , satisfy
  , stream

  , parseCBS
  , parseSignedInt

  , listlistParser
  , parseIntList
  ) where

import Control.Arrow (first)
import Control.Applicative (Alternative (..))
import Data.Char (isDigit, isSpace)

--Task1-------------------------------------------------------------------

-- | Wrapper for parser combinators.
newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser $ fmap (first f) . parser

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) (Parser pf) (Parser pa) = Parser $ \s -> do
    (dataF, restF) <- pf s
    (dataA, restA) <- pa restF
    return (dataF dataA, restA)

instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser parser) f = Parser $ \s -> do
    (result, rest) <- parser s
    let (Parser nParser) = f result
    nParser rest

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser p1) (Parser p2) = Parser $ \s -> p1 s <|> p2 s

--Task2-------------------------------------------------------------------

-- | Parser which never fails and never consumes input.
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- | Parser which checks whether end of input is reached.
eof :: Parser s ()
eof = Parser $ \s -> case s of
  [] -> Just ((), s)
  _  -> Nothing

-- | Parser which checks whether next element of input
-- satisfy the given predicate and consumes element.
-- Fails otherwise.
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \s -> case s of
  []     -> Nothing
  (x:xs) -> if p x
            then Just (x, xs) 
            else Nothing

-- | Parser which checks whether next element from input
-- is equal to given and consumes element. Fails otherwise.
element :: Eq s => s -> Parser s s
element c = satisfy (== c)

-- | Parser which checks whether prefix of input is equal
-- to given list of elements and consumes these elements.
-- Fails otherwise.
stream :: Eq s => [s] -> Parser s [s]
stream = traverse element

--Task3-------------------------------------------------------------------

-- | Checks whether given string is correct brace sequence.
-- Returns True if yes, False otherwise.
parseCBS :: String -> Bool
parseCBS s = 
  let 
    brParser :: Parser Char ()
    brParser = sqParser <* eof

    sqParser :: Parser Char ()
    sqParser = (element '(' *> sqParser *> element ')' *> sqParser) <|> ok
  in
    case runParser brParser s of
      Nothing -> False
      _       -> True

-- | Trying to parse given string as signed integer.
-- On success returns Just result, Nothing otherwise.
parseSignedInt :: String -> Maybe Integer
parseSignedInt s = fmap fst (runParser signedIntEofParser s)

signedIntParser :: Parser Char Integer
signedIntParser = 
  let 
    signToMult :: Char -> (Integer -> Integer)
    signToMult '+' = (*) 1
    signToMult '-' = (*) (-1)
    signToMult _   = error "Unknown sign"
  in
    do
      sign   <- signParser
      number <- intParser
      case sign of
        Left  _  -> return number
        Right ch -> return $ signToMult ch number 
      

signParser :: Parser Char (Either () Char)
signParser =  fmap Right (element '-' <|> element '+') <|> fmap Left ok

signedIntEofParser :: Parser Char Integer
signedIntEofParser = signedIntParser <* eof

intParser :: Parser Char Integer
intParser = Parser helper
  where
    helper :: [Char] -> Maybe (Integer, [Char])
    helper xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

--Task4-------------------------------------------------------------------

-- | Trying to parse given string as list of integer lists
-- in the following format. 
-- >>> "2, 1,+10  , 3,5,-7, 2"
-- [ [1, 10], [5, -7, 2] ]
parseIntList :: String -> [[Integer]]
parseIntList s = case runParser listlistParser s of
  Nothing       -> error "Incorrect input"
  Just (res, _) -> res

-- | Parser which is used to parse list of integer lists.
listlistParser :: Parser Char [[Integer]]
listlistParser = listListHelper []
  where
    listListHelper :: [[Integer]] -> Parser Char [[Integer]]
    listListHelper listOfList = do
      skipWhitespaces
      listLen <- signedIntParser
      skipWhitespaces
      list <- parseNInts listLen
      skipWhitespaces
      let nListOfLists = listOfList ++ [list]
      isEnd <- hasEofOccured
      if isEnd
      then return nListOfLists
      else 
        do
          _ <- element ',' 
          listListHelper nListOfLists

skip :: (a -> Bool) -> Parser a ()
skip p = do
  result <- fmap Right (satisfy p) <|> fmap Left ok
  case result of
    Left  _ -> return ()
    Right _ -> skip p

skipWhitespaces :: Parser Char ()
skipWhitespaces = skip isSpace

parseNInts :: Integer -> Parser Char [Integer]
parseNInts n 
  | n < 0     = Parser $ \_ -> Nothing
  | otherwise = nIntsHelper n [] 
  where
    nIntsHelper :: Integer -> [Integer] -> Parser Char [Integer]
    nIntsHelper 0 list = return list
    nIntsHelper m list = do
      skipWhitespaces
      _ <- element ','
      skipWhitespaces
      int <- signedIntParser
      skipWhitespaces
      nIntsHelper (m - 1) (list ++ [int])

hasEofOccured :: Parser Char Bool
hasEofOccured = do
  result <- fmap Right eof <|> fmap Left ok 
  case result of
    Left  _ -> return False
    Right _ -> return True
