-- | Semigroup.hs

module Chapter15.SemiGroup where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Eq, Show)

newtype Identity a = Identity a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

newtype BoolConj = BoolConj Bool
newtype BoolDisj = BoolDisj Bool

data Or a b =
  Fst a
  | Snd b

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

newtype Comp a =
  Comp { unComp :: (a -> a) }

data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity x') = Identity $ x <> x'

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  (Fst _) <> (Snd b) = Snd b
  _ <> x' = x'

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine f') = Combine (\n -> (f n) <> (f' n))

instance Semigroup (Comp a) where
  (Comp f) <> (Comp f') = Comp (\n -> f $ f' n)

instance Semigroup a => Semigroup (Validation a b) where
  (Success' b) <> _ = Success' b
  (Failure' _) <> (Success' b) = Success' b
  (Failure' x) <> (Failure' x') = Failure' $ x <> x'

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
