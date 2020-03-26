module Block1_Task1Spec 
  ( fullSpec
  ) where

import Test.Hspec (SpecWith, errorCall, describe, it, shouldBe, shouldThrow)
import Control.Exception (evaluate)

import Block1_Task1

fullSpec :: SpecWith ()
fullSpec = 
  do
    nextDaySpec
    afterDaysSpec
    isWeekendSpec
    daysToPartySpec

nextDaySpec :: SpecWith ()
nextDaySpec = 
  describe "Block1_Task1.nextDay" $ do
    it "Next day for Monday is Tuesday" $ do
      (nextDay Monday) `shouldBe` Tuesday
    it "Next day for Tuesday is Wednesday" $ do
      (nextDay Tuesday) `shouldBe` Wednesday
    it "Next day for Wednesday is Thursday" $ do
      (nextDay Wednesday) `shouldBe` Thursday
    it "Next day for Thursday is Friday" $ do
      (nextDay Thursday) `shouldBe` Friday
    it "Next day for Friday is Saturday" $ do
      (nextDay Friday) `shouldBe` Saturday
    it "Next day for Saturday is Sunday" $ do
      (nextDay Saturday) `shouldBe` Sunday
    it "Next day for Sunday is Monday" $ do
      (nextDay Sunday) `shouldBe` Monday

afterDaysSpec :: SpecWith ()
afterDaysSpec =
  describe "Block1_Task1.afterDays" $ do
    it "Negative number of days is forbidden" $ do
      evaluate (afterDays Monday (-1)) `shouldThrow` errorCall "Negative number of days"
    it "If number of days 0, returns same day" $ do
      (afterDays Sunday 0) `shouldBe` Sunday
    it "The afterDays Tuesday 4 is Saturday" $ do
      (afterDays Tuesday 4) `shouldBe` Saturday
    it "The afterDays Friday 13 is Thursday" $ do
      (afterDays Friday 13) `shouldBe` Thursday

isWeekendSpec :: SpecWith ()
isWeekendSpec =
  describe "Block1_Task1.isWeekend" $ do
    it "Saturday is holiday" $ do
      (isWeekend Saturday) `shouldBe` True
    it "Sunday is holiday" $ do
      (isWeekend Sunday) `shouldBe` True
    it "Monday isn't holiday" $ do
      (isWeekend Monday) `shouldBe` False

daysToPartySpec :: SpecWith ()
daysToPartySpec =
  describe "Block1_Task1.daysToParty" $ do
    it "If it is Friday, answer is 0" $ do
      (daysToParty Friday) `shouldBe` 0
    it "If is is Sunday, answer is 6" $ do
      (daysToParty Sunday) `shouldBe` 5
    it "If it is Wednesday, answer is 2" $ do
      (daysToParty Wednesday) `shouldBe` 2