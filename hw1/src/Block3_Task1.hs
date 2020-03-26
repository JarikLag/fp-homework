module Block3_Task1 
  ( eitherConcat
  , maybeConcat
  ) where

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat list = helper list []
  where
    helper :: [Maybe [a]] -> [a] -> [a]
    helper []     acc = acc
    helper (x:xs) acc = case x of
      Just y  -> helper xs (acc ++ y)
      Nothing -> helper xs acc

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a,b)
eitherConcat list = helper list (mempty, mempty)
  where
    helper :: (Monoid a, Monoid b) => [Either a b] -> (a, b) -> (a,b)
    helper []     acc   = acc
    helper (x:xs) (f,s) = case x of
      Left a  -> helper xs (f <> a, s)
      Right b -> helper xs (f, s <> b) 