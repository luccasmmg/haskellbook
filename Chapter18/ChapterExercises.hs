-- | ChapterExercises.hs

module Chapter18.ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad (join, liftM2, liftM)

data Nope a =
  NopeDotJpg deriving (Eq, Show, Ord)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

data PhhhbbtttEither b a =
  Left' a
  | Right' b
  deriving (Eq, Show, Ord)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' (f a)

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (Left' f) <*> (Left' a) = Left' $ f a
  (Right' b) <*> _ = Right' b
  _ <*> (Right' b) = Right' b

instance Monad (PhhhbbtttEither b) where
  return = Left'
  Left' a >>= f = f a
  Right' b >>= _ = Right' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
  arbitrary = genPhhhbbtttEither

genPhhhbbtttEither :: (Arbitrary a, Arbitrary b) => Gen (PhhhbbtttEither a b)
genPhhhbbtttEither = do
  a <- arbitrary
  b <- arbitrary
  frequency [ (1, return (Right' b))
            , (3, return (Left' a))]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where
  (=-=) = eq

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)
 
instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  return (Identity a)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq
 
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = (Cons (f a) (fmap f as))

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure f = Cons f Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= f = append (f x) (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [pure Nil, Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where
  (=-=) = eq

main = do
  let trigger :: Nope (Int, String, Int)
      trigger = undefined
  let trigger' :: PhhhbbtttEither (Int, String, Int) (Int, String, Int)
      trigger' = undefined
  let trigger'' :: Identity (Int, String, Int)
      trigger'' = undefined
  let trigger''' :: List (Int, String, Int)
      trigger''' = undefined
  quickBatch $ monad trigger
  quickBatch $ monad trigger'
  quickBatch $ monad trigger''
  quickBatch $ monad trigger'''

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a x b = b <*> x

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (m:ms) f = do
  x <- (f m)
  xs <- meh ms f
  return (x:xs)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs (\x -> x)
