{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity expr = case expr of
  Left  a      -> (Left a, Left a)
  Right (b, c) -> (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (left, right)
  where
    left :: Either a (Either b c) -> Either (Either a b) c
    left expr = case expr of
      Left  a  -> Left $ Left a
      Right rt -> case rt of 
        Left  b -> Left $ Right b
        Right c -> Right c
    right :: Either (Either a b) c -> Either a (Either b c)
    right expr = case expr of
      Left lt -> case lt of
        Left  a -> Left a
        Right b -> Right $ Left b
      Right c -> Right $ Right c