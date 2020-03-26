module Block6Spec 
  ( fullSpec
  ) where

import Test.Hspec (SpecWith, errorCall, describe, it, shouldBe, shouldThrow)
import Control.Exception (evaluate)
import Test.QuickCheck (property)

import Block6

fullSpec :: SpecWith ()
fullSpec = 
  do
    okParserSpec
    eofParserSpec
    satisfyParserSpec
    elementParserSpec
    streamParserSpec
    parseCBSSpec
    parseSignedIntSpec
    parseIntListSpec

okParserSpec :: SpecWith ()
okParserSpec =
  describe "Block6.ok" $ do
    it "OkParser parses every string" $ do
      property checkEveryString
  where
    checkEveryString :: String -> Bool
    checkEveryString s = runParser ok s == Just ((), s)

eofParserSpec :: SpecWith ()
eofParserSpec =
  describe "Block6.eof" $ do
    it "EofParser parses only eof" $ do
      runParser eof "" `shouldBe` Just ((), "")
    it "EofParser fails on non-empty input" $ do
      runParser eof "notEmptyString" `shouldBe` Nothing

satisfyParserSpec :: SpecWith ()
satisfyParserSpec =
  describe "Block6.satisfy" $ do
    it "SatisfyParser parses signle element" $ do
      runParser (satisfy (=='a')) "axyz" `shouldBe` Just ('a', "xyz")
    it "SatisfyParser fails if first element doesn't match" $ do
      runParser (satisfy (=='a')) "xyz" `shouldBe` Nothing

elementParserSpec :: SpecWith ()
elementParserSpec =
  describe "Block6.element" $ do
    it "ElementParser parses signle element" $ do
      runParser (element 'a') "abac" `shouldBe` Just ('a', "bac")
    it "ElementParser fails if first element doesn't match" $ do
      runParser (element 'c') "abacaba" `shouldBe` Nothing

streamParserSpec :: SpecWith ()
streamParserSpec =
  describe "Block6.stream" $ do
    it "StreamParser parses start of the string" $ do
      runParser (stream "aba") "abacaba" `shouldBe` Just ("aba", "caba")
    it "StreamParser fails if start of the string doesn't match" $ do
      runParser (stream "aba") "notabacaba" `shouldBe` Nothing
    it "StreamParser parses start of every string" $ do
      property checkEveryString
  where
    checkEveryString :: String -> String -> Bool
    checkEveryString prefix suffix = runParser (stream prefix) (prefix ++ suffix) == Just (prefix, suffix)

parseCBSSpec :: SpecWith ()
parseCBSSpec =
  describe "Block6.parseCBS" $ do
    it "Parses empty string" $
      parseCBS "" `shouldBe` True
    it "Parses simple CBS" $ 
      parseCBS "()" `shouldBe` True
    it "Fails if any other symbols are present #1" $
      parseCBS "(())   ()" `shouldBe` False
    it "Fails if any other symbols are present #2" $
      parseCBS "(((a)(b)))" `shouldBe` False
    it "Parses big CBS" $ 
      parseCBS "()()()((()()))((((()()()))))" `shouldBe` True

parseSignedIntSpec :: SpecWith ()
parseSignedIntSpec =
  describe "Block6.parseSignedInt" $ do
    it "Parses any number" $
      property checkEveryNumber
    it "Fails if non-parsed symbols left" $ do
      property checkEveryNumberNotEof
  where
    checkEveryNumber :: Integer -> Bool
    checkEveryNumber x = (parseSignedInt $ show x) == (Just x)

    checkEveryNumberNotEof :: Integer -> String -> Bool
    checkEveryNumberNotEof x s = (parseSignedInt (show x ++ " " ++ s)) == Nothing

parseIntListSpec :: SpecWith ()
parseIntListSpec = 
  describe "Block6.parseIntList" $ do
    it "Parses random input #1" $ do
      parseIntList "1, 1" `shouldBe` [[1]]
    it "Parses random input #2" $ do
      parseIntList "2, 1,+10  , 3,5,-7, 2" `shouldBe` [[1, 10], [5, -7, 2]]
    it "Parses random input #3" $ do
      parseIntList "2, 01,+0010 \n , +03,5,-7, 2" `shouldBe` [[1, 10], [5, -7, 2]]
    it "Parses random input #4" $ do
      parseIntList "2, 01,+0010 \n , +03, \n\n\n -5,  \t\t -7, 2    \n \n\r\t" 
      `shouldBe` [[1, 10], [-5, -7, 2]]
    it "Fails on incorrect input #1" $ do
      evaluate (parseIntList "-1, 1") `shouldThrow` errorCall "Incorrect input"
    it "Fails on incorrect input #1" $ do
      evaluate (parseIntList "2, 1, 3, 4") `shouldThrow` errorCall "Incorrect input"