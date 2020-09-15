{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Task3 
  ( HScript (..)
  , interpretScript
  ) where

import Control.Monad.ST
  ( ST
  , runST
  )
import Control.Monad.State
  ( StateT
  , evalStateT
  , get
  , lift
  , put)
import Data.HashMap.Strict as Map
  ( HashMap
  , (!)
  , empty
  , insert
  , size
  )
import Data.STRef
  ( STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  )
import Data.Typeable
  ( Typeable
  , cast
  )

data Obj where
  Obj :: Typeable a => a -> Obj

data Store s
  = Store 
  { varRefs :: Map.HashMap Int (STRef s Obj)
  }

newtype VarRef a = VarRef Int
 
type STInterpret s = StateT (Store s) (ST s)

class (Typeable var, Ord var, Show var) => HVar var

instance HVar Int
instance HVar Double
instance HVar Bool

-- | Type class to represent HalyavaScript language.
class HScript script where
  (@=)  :: HVar a => VarRef a -> a -> script ()
  (@>)  :: HVar a => VarRef a -> a -> script Bool
  (@>=) :: HVar a => VarRef a -> a -> script Bool
  (@<)  :: HVar a => VarRef a -> a -> script Bool
  (@<=) :: HVar a => VarRef a -> a -> script Bool
  (@==) :: HVar a => VarRef a -> a -> script Bool
  (#)   :: script a -> script b -> script b

  sWithVar1 :: HVar a 
            => a 
            -> (VarRef a -> script b) 
            -> script b

  sWithVar2 :: (HVar a, HVar c)
            => a 
            -> c 
            -> (VarRef a -> VarRef c -> script b) 
            -> script b

  sReadVar1 :: HVar a 
            => VarRef a 
            -> (a -> script b) 
            -> script b
  
  sReadVar2 :: (HVar a, HVar c)
            => VarRef a 
            -> VarRef c 
            -> (a -> c -> script b) 
            -> script b
  
  sWhile    :: script Bool 
            -> script a 
            -> script ()
  
  sIf       :: script Bool 
            -> script a 
            -> script a 
            -> script a
  
  sFun1     :: (HVar a, HVar b)
            => b 
            -> (VarRef a -> VarRef b -> script c) 
            -> a 
            -> script b
  
  sFun2     :: (HVar a, HVar b, HVar c)
            => c 
            -> (VarRef a -> VarRef b -> VarRef c -> script d) 
            -> a 
            -> b 
            -> script c

infixr 0 #
infixr 2 @=
infixr 2 @>
infixr 2 @>=
infixr 2 @<
infixr 2 @<=
infixr 2 @==

updVarRef :: (HashMap Int (STRef s1 Obj) -> HashMap Int (STRef s2 Obj))
          -> Store s1 
          -> Store s2
updVarRef f s@Store{ varRefs = rs } = s { varRefs = f rs }

allocNewVar :: HVar a
            => a 
            -> STInterpret s (VarRef a)
allocNewVar a = do
  store <- get
  let i = Map.size $ varRefs store
  let o = Obj a
  refV1 <- lift $ newSTRef o
  let nStore = updVarRef (Map.insert i refV1) store
  put nStore
  return $ VarRef i

extractVar :: HVar a
           => VarRef a -> STInterpret s a
extractVar (VarRef i) = do
  store <- get
  let ref = (Map.! i) $ varRefs store
  (Obj val) <- lift $ readSTRef ref
  let val' = case cast val of
              Nothing  -> error $ "Can't perform cast"
              (Just b) -> b
  return val'

compareVals :: HVar a => VarRef a -> a -> (a -> a -> Bool) -> STInterpret s Bool
compareVals varRef v2 cmp = do
    v1 <- extractVar varRef
    return $ v1 `cmp` v2

instance HScript (STInterpret s) where
  (@=) :: HVar a => VarRef a -> a -> STInterpret s ()
  (@=) (VarRef i) val = do
    store <- get
    let ref = (Map.! i) $ varRefs store
    lift $ writeSTRef ref (Obj val)
    let nStore = updVarRef (Map.insert i ref) store
    put nStore

  (@>) :: HVar a => VarRef a -> a -> STInterpret s Bool
  (@>) varRef v2 = compareVals varRef v2 (>)

  (@>=) :: HVar a => VarRef a -> a -> STInterpret s Bool
  (@>=) varRef v2 = compareVals varRef v2 (>=)

  (@<) :: HVar a => VarRef a -> a -> STInterpret s Bool
  (@<) varRef v2 = compareVals varRef v2 (<)

  (@<=) :: HVar a => VarRef a -> a -> STInterpret s Bool
  (@<=) varRef v2 = compareVals varRef v2 (<=)

  (@==) :: HVar a => VarRef a -> a -> STInterpret s Bool
  (@==) varRef v2 = compareVals varRef v2 (==)

  (#) :: STInterpret s a -> STInterpret s b -> STInterpret s b
  (#) = (>>)

  sWithVar1 :: HVar a 
            => a
            -> (VarRef a -> STInterpret s b) 
            -> STInterpret s b
  sWithVar1 val f = do
    ref <- allocNewVar val
    f ref

  sWithVar2 :: (HVar a, HVar b)
            => a 
            -> b
            -> (VarRef a -> VarRef b -> STInterpret s c) 
            -> STInterpret s c
  sWithVar2 val1 val2 f = do
    ref1 <- allocNewVar val1
    ref2 <- allocNewVar val2
    f ref1 ref2

  sReadVar1 :: HVar a 
            => VarRef a 
            -> (a -> STInterpret s b) 
            -> STInterpret s b
  sReadVar1 ref f = do
    val <- extractVar ref
    f val

  sReadVar2 :: (HVar a, HVar c)
            => VarRef a 
            -> VarRef c 
            -> (a -> c -> STInterpret s b) 
            -> STInterpret s b
  sReadVar2 ref1 ref2 f = do
    val1 <- extractVar ref1
    val2 <- extractVar ref2
    f val1 val2

  sWhile :: STInterpret s Bool 
         -> STInterpret s a 
         -> STInterpret s ()
  sWhile cond body = do
    isTrue <- cond
    if isTrue
    then do
      _ <- body
      sWhile cond body
    else return ()

  sIf :: STInterpret s Bool 
      -> STInterpret s a 
      -> STInterpret s a 
      -> STInterpret s a
  sIf cond ifBody elseBody = do
    isTrue <- cond
    if isTrue
    then ifBody
    else elseBody

  sFun1 :: (HVar a, HVar b)
        => b 
        -> (VarRef a -> VarRef b -> STInterpret s c) 
        -> a 
        -> STInterpret s b
  sFun1 out f inp = do
    refInp <- allocNewVar inp
    refOut <- allocNewVar out
    _ <- f refInp refOut
    extractVar refOut
  
  sFun2 :: (HVar a, HVar b, HVar c)
        => c 
        -> (VarRef a -> VarRef b -> VarRef c -> STInterpret s d) 
        -> a 
        -> b 
        -> STInterpret s c
  sFun2 out f inp1 inp2 = do
    refInp1 <- allocNewVar inp1
    refInp2 <- allocNewVar inp2
    refOut <- allocNewVar out
    _ <- f refInp1 refInp2 refOut
    extractVar refOut 

-- | Function to interpret code of HalyavaScript.
interpretScript :: (forall s. STInterpret s a) -> a
interpretScript script = runST $ evalStateT script initState
  where
    initState :: Store s
    initState = Store { varRefs = Map.empty }