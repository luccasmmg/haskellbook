-- | EitherMonadExercise.hs

module Chapter18.EitherMonadExercise where

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (Second f) <*> (Second a) = Second $ f a
  (First e) <*> _ = First e
  _ <*> (First e) = First e

instance Monad (Sum a) where
  return = pure
  (Second a) >>= f = f a
  (First e) >>= _ = First e
