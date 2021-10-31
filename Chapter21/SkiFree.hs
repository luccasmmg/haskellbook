{-# LANGUAGE FlexibleContexts #-}

module Chapter21.SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n, Arbitrary (n a), Arbitrary a ) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n
        , Testable (n Property)
        , EqProp a )
        => EqProp (S n a) where
  (S x y) =-= (S p q) =
    (property $ (=-=) <$> x <*> p)
    .&. (y =-= q)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
--foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x y z) = (Node (fmap f x) (f y) (fmap f z))

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node x y z) = (foldMap f x) <> (f y) <> (foldMap f z)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> (f x)
  traverse f (Node x y z) = Node <$> traverse f x <*> f y <*> traverse f z
