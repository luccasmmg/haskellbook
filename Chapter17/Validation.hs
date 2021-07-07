-- | Validation.hs

module Chapter17.Validation where

data Errors =
    DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  (Success f) <*> (Success a) = Success (f a)
  (Failure e) <*> (Failure e') = Failure $ e <> e'
  (Failure e) <*> _ = Failure e
  _ <*> (Failure e) = Failure e
