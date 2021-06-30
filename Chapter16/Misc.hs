{-# LANGUAGE MultiParamTypeClasses #-}
-- | Misc.hs

module Chapter16.Misc where

class Sumthin a where
  s :: a -> a

class Else where
  e :: b -> f (g a b c)

class Biffy where
  slayer :: e a b -> (a -> c) -> (b -> d) -> e c d

-- class Impish v where
--   impossibleKind :: v -> v a

-- class AlsoImp v where
--   nope :: v a -> v

data FixMePls a =
  FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

data WhoCares a =
  ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

data CountingBad a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n) (f a)

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m


incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' = fmap (+1)

data Possibly a =
  LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

getInt :: IO Int
getInt = fmap read getLine
