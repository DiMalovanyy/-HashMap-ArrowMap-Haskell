module Main where


import Prelude hiding (lookup)
import Data.Monoid

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance Semigroup (ListMap a b) where
  ListMap []  <>  ListMap [] = ListMap []
  ListMap []  <>  ListMap a  = ListMap a
  ListMap a   <>  ListMap [] = ListMap a
  ListMap a   <>  ListMap b  = ListMap (a <> b)

instance Monoid (ListMap a b) where
  mempty = ListMap []


instance MapLike ListMap where
  empty = ListMap []

  lookup _ (ListMap [])  = Nothing
  lookup k' (ListMap ((k,v) : xs)) | k == k' = Just v
                                   | otherwise = lookup k' (ListMap xs)

  delete _ (ListMap []) = ListMap []
  delete k' (ListMap ((k, v) : xs)) | k == k' = delete k' (ListMap xs)
                                    | otherwise = foldr mappend mempty [(ListMap ((k,v) : [])) , (delete k' (ListMap xs))]

  insert k v (ListMap []) = ListMap ((k,v) : [])
  insert k v (ListMap ((k', v') : xs)) | k == k' = foldr mappend mempty [ListMap ((k,v) : []), ListMap xs]
                                       | k < k' = foldr mappend mempty [ListMap ((k,v) : []), ListMap((k',v') : []), ListMap xs]
                                       | otherwise = foldr mappend mempty [ListMap ((k',v') : []) , insert k v (ListMap xs)]


newtype ArrowMap  k v = ArrowMap { getArrowMap :: k -> Maybe v }


instance MapLike ArrowMap  where

  empty = ArrowMap  (\_ -> Nothing)

  lookup k (ArrowMap f) = f k

  delete k (ArrowMap f) = ArrowMap (\x -> if (x == k) then Nothing else (f x))

  insert k v (ArrowMap  f) = ArrowMap (\x -> if (x == k) then (Just v) else (f x))


newtype Endo' a = Endo' {appEndo :: a -> a}

instance Semigroup (Endo' a) where
  Endo' a <> Endo' b = Endo' (a . b)

instance Monoid (Endo' a) where
  mempty = Endo' id



-- instance Semigroup (ArrowMap k v) where
--
--
-- instance Monoid (ArrowMap k v) where
--   mempty = ArrowMap id




main :: IO ()
main = print ("Arrow Map")
