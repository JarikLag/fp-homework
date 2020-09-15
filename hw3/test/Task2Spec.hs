{-# LANGUAGE BangPatterns #-}

module Task2Spec
  ( fullSpec
  ) where

import Task2 
  ( ConcurrentHashTable
  , getCHT
  , newCHT
  , putCHT
  , sizeCHT
  )
import Control.Concurrent
  ( forkIO
  , newEmptyMVar
  , putMVar
  , readMVar
  )
import Data.Foldable 
  ( for_
  )
import Test.Hspec 
  ( SpecWith
  , describe
  , it
  , shouldBe
  )

fullSpec :: SpecWith ()
fullSpec = do
  describe "Task2 test" $ do
    it "Performs PUT from more than 1 thread" $ do
      h <- newCHT :: IO (ConcurrentHashTable String Int)
      let cntOp = [1..100]
      mvar1 <- newEmptyMVar
      mvar2 <- newEmptyMVar
      _ <- forkIO $ do
        for_ cntOp $ \i -> putCHT ("thread1 " ++ show i) i h
        putMVar mvar1 ()
      _ <- forkIO $ do
        for_ cntOp $ \i -> putCHT ("thread2 " ++ show (2 * i)) (2 * i) h
        putMVar mvar2 ()
      !_ <- readMVar mvar1
      !_ <- readMVar mvar2
      size <- sizeCHT h
      size `shouldBe` 200

    it "Performs GET from more than 1 thread" $ do
      h <- newCHT :: IO (ConcurrentHashTable String Int)
      let cntOp = [1..100]
      mvar1 <- newEmptyMVar
      mvar2 <- newEmptyMVar
      for_ cntOp $ \i -> putCHT ("Value#" ++ show i) i h
      _ <- forkIO $ do
        for_ cntOp $ \i -> do 
          val <- getCHT ("Value#" ++ show i) h
          val `shouldBe` (Just i)
        putMVar mvar1 ()
      _ <- forkIO $ do
        for_ cntOp $ \i -> do 
          val <- getCHT ("Value#" ++ show i) h
          val `shouldBe` (Just i)
        putMVar mvar2 ()
      !_ <- readMVar mvar1
      !_ <- readMVar mvar2
      size <- sizeCHT h
      size `shouldBe` 100 