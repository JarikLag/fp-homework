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

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \s -> case s of
  [] -> Just ((), s)
  _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \s -> case s of
  []     -> Nothing
  (x:xs) -> if p x
            then Just (x, xs) 
            else Nothing

element :: Eq s => s -> Parser s s
element c = satisfy (== c)

stream :: Eq s => [s] -> Parser s [s]
stream = traverse element

--Task3-------------------------------------------------------------------

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

parseIntList :: String -> [[Integer]]
parseIntList s = case runParser listlistParser s of
  Nothing       -> error "Incorrect input"
  Just (res, _) -> res

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
