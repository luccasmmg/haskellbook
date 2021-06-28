-- | InstancesOfFunc.hs

module Chapter16.InstancesOfFunc where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

data Pair a = Pair a a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

data Three' a b = Three' a b b deriving (Eq, Show)

data Four a b c d = Four a b c d deriving (Eq, Show)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Four' a a a b)

type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool
type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool
type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool
type Three'FC = Three' Int Int -> IntToInt -> IntToInt -> Bool
type FourFC = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool
type Four'FC = Four' Int Int -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
  quickCheck (functorCompose' :: IdentityFC)
  quickCheck (functorCompose' :: PairFC)
  quickCheck (functorCompose' :: TwoFC)
  quickCheck (functorCompose' :: ThreeFC)
  quickCheck (functorCompose' :: Three'FC)
  quickCheck (functorCompose' :: FourFC)
  quickCheck (functorCompose' :: Four'FC)
