module Task3Spec
  ( fullSpec
  ) where

import Task3
  ( HScript (..)
  , interpretScript
  )
import Test.Hspec 
  ( SpecWith
  , describe
  , it
  , shouldBe
  )

fullSpec :: SpecWith ()
fullSpec = do
  describe "Task3 test" $ do
    log2Spec
    isEvenSpec

log2Spec :: SpecWith ()
log2Spec = do
  describe "log2" $ do
    it "log2(1) == 0" $
      interpretScript (log2 1) `shouldBe` 0
    it "log2(2) == 1" $
      interpretScript (log2 2) `shouldBe` 1
    it "log2(5) == 3" $
      interpretScript (log2 5) `shouldBe` 3
    it "log2(63) == 6" $
      interpretScript (log2 63) `shouldBe` 6

isEvenSpec :: SpecWith ()
isEvenSpec = do
  describe "isEven" $ do
    it "isEven(1) == False" $
      interpretScript (isEven 1) `shouldBe` False
    it "isEven(2) == True" $
      interpretScript (isEven 2) `shouldBe` True
    it "isEven(-1) == False" $
      interpretScript (isEven (-1)) `shouldBe` False

log2 :: HScript script => Int -> script Int
log2 =
  sFun1 0 $ \a logCnt ->
  sWithVar1 0 $ \accum ->
    accum @= 1 #
    logCnt @= 0 #
    sWhile (sReadVar1 accum $ \eAccum -> a @> eAccum)
      ( sReadVar2 accum logCnt $ \eAccum eLogCnt ->
          accum @= eAccum + eAccum #
          logCnt @= eLogCnt + 1
      )

isEven :: HScript script => Int -> script Bool
isEven =
  sFun1 False $ \num res ->
  sReadVar1 num $ \eNum ->
    num @= abs(eNum `mod` 2) #
    sIf (num @== 0)
      ( res @= True )
      ( res @= False )