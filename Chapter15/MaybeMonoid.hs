-- | MaybeMonoid.hs

module Chapter15.MaybeMonoid where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada

instance Semigroup (First' a) where
  (First' Nada) <> x = x
  x <> _ = x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada)
                        ,(3, liftM Only arbitrary)]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = fmap First' arbitrary

firstMappend :: First' a -> First' a -> First' a
firstMappend = (<>)

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
