module Block3_Task1 
  ( eitherConcat
  , maybeConcat
  ) where

-- | Takes list of list inside Maybe and returns
-- their concatentaion.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat list = helper list []
  where
    helper :: [Maybe [a]] -> [a] -> [a]
    helper []     acc = acc
    helper (x:xs) acc = case x of
      Just y  -> helper xs (acc ++ y)
      Nothing -> helper xs acc

-- | Takes list of (Either a b), where a and b are Monoids,
-- returns pair (c, d), where c is mappend of all elements
-- inside Left, d is mappend of all elements inside Right.
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a,b)
eitherConcat list = helper list (mempty, mempty)
  where
    helper :: (Monoid a, Monoid b) => [Either a b] -> (a, b) -> (a,b)
    helper []     acc   = acc
    helper (x:xs) (f,s) = case x of
      Left a  -> helper xs (f <> a, s)
      Right b -> helper xs (f, s <> b) 