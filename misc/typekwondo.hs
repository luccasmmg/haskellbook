-- | Type Kwon Do

module Typekwondo where

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday'
    && dayOfMonth == dayOfMonth'

data Identity a = Identity a
instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn number)
       (TisAn number') =
    number == number'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two a b)
       (Two a' b') =
    a == a' && b == b'

data StringOrInt =
  TisAnInt Int
  | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt v) (TisAnInt v') = v == v'
  (==) (TisAString s) (TisAString s') = s == s'
  (==) _ _ = False

data Pair a =
  Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') =
    x == x' && y == y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') =
    x == x' && y == y'

data Which a =
  ThisOne a
  | ThatOne a
instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

data EitherOr a b =
  Hello a
  | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False
