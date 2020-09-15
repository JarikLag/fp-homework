{-# LANGUAGE BangPatterns #-}

module Task2
  ( ConcurrentHashTable (..)
  , getCHT
  , newCHT
  , putCHT
  , sizeCHT
  ) where

import Control.Concurrent.STM 
  ( STM
  , TVar
  , atomically
  , readTVar
  , newTVar
  , writeTVar
  )
import Data.Hashable 
  ( Hashable
  , hash
  )
import Data.Foldable 
  ( forM_
  )
import qualified Data.Vector as Vector
  ( Vector
  , (!)
  , length
  , replicateM
  , toList
  )

type StorageBucket k v = TVar [(k,v)]

data ConcurrentHashTable k v 
  = ConcurrentHashTable
  { tableSize :: TVar Int
  , tableStorage :: TVar (Vector.Vector (StorageBucket k v))
  }

defaultStorageSize :: Int
defaultStorageSize = 1024

maxLoadCoeff :: Double
maxLoadCoeff = 0.75

calculateLoad :: Int -> Int -> Double
calculateLoad cur total = (fromIntegral cur) / (fromIntegral total)

getBucket :: Hashable k => ConcurrentHashTable k v -> k -> STM (StorageBucket k v)
getBucket h k = do
  storage <- readTVar $ tableStorage h
  return $! storage Vector.! (hash k `mod` (Vector.length storage))

createEmptyStorage :: Int ->  STM (Vector.Vector (StorageBucket k v))
createEmptyStorage sz = Vector.replicateM sz (newTVar [])

toList :: ConcurrentHashTable k v -> STM [(k, v)]
toList h = do
  storage <- readTVar $ tableStorage h
  listOfBuckets <- mapM readTVar $ Vector.toList storage
  return $ concat listOfBuckets

addItem :: (Hashable k, Ord k) => k -> v -> ConcurrentHashTable k v -> STM ()
addItem k v t = do
  bucket <- getBucket t k
  l <- readTVar bucket
  let l' = (k, v) : l
  writeTVar bucket $! l'

rehashTable :: (Hashable k, Ord k) => (k, v) -> ConcurrentHashTable k v -> STM ()
rehashTable extra t = do
  entries <- toList t
  let oldSize = length entries
  newStorage <- createEmptyStorage (oldSize * 2)
  writeTVar (tableStorage t) newStorage
  forM_ (extra : entries) (\(k',v') -> addItem k' v' t)
  writeTVar (tableSize t) (oldSize + 1)

newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  size' <- newTVar 0
  storage' <- createEmptyStorage defaultStorageSize
  storageTVar <- newTVar storage'
  return $ ConcurrentHashTable size' storageTVar

-- | Performs lookup into given HashTable 
getCHT :: (Hashable k, Ord k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT !k t = do
 list <- atomically $ do
   bucket <- getBucket t k
   readTVar bucket
 return $ lookup k list

-- | Adds pair key-value into given HashTable.
putCHT :: (Hashable k, Ord k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT !k !v t = atomically $ do
  size' <- readTVar $ tableSize t
  storage <- readTVar $ tableStorage t
  if calculateLoad size' (Vector.length storage) >= maxLoadCoeff
  then rehashTable (k, v) t
  else do
    addItem k v t
    writeTVar (tableSize t) (size' + 1)

-- | Returns amount of elements in given HashTable.
sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT t = atomically $ readTVar (tableSize t)