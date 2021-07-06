-- |

module Chapter17.ApplicativeList where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (List a) where (=-=) = eq

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = (Cons (f a) (fmap f as))

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

-- instance Applicative List where
--   pure f = Cons f Nil
--   (<*>) _ Nil = Nil
--   (<*>) Nil _ = Nil
--   (<*>) (Cons f fs) (Cons a as) = (Cons (f a) ((<*>) fs as))

instance Applicative List where
  pure f = Cons f Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

type ListInt = List Integer
