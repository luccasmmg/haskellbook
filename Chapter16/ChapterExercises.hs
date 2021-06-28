{-# LANGUAGE FlexibleInstances #-}}
-- | ChapterExercises.hs

module Chapter16.ChapterExercises where

data Sum a b =
  First a
  | Second b

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

data Company a b c =
  DeepBlue a c
  | Something b

instance Functor (Company a b) where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)

data More b a =
  L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b =
  Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K a b =
  K a

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a (List a)) = (Cons (f a) (fmap (List a)))

data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats x y z) = MoreGoats $ (fmap f x) (fmap f y) (fmap f z)
