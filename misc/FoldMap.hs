-- | FoldMap

module FoldMap where

import Data.Monoid

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l k r) = foldMap f l <> f k <> foldMap f r

newtype Max a = Max { getMax :: a } deriving (Eq, Ord, Show)

instance (Ord a, Num a) => Semigroup (Max a) where
  (Max a) <> (Max a') = Max $ if a > a' then a else a'

instance (Ord a, Num a) => Monoid (Max a) where
  mempty = Max $ 0

tree = Node (Leaf 1) 7 (Leaf 30)

ex = foldMap Max tree
