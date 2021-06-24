module Listy where

newtype Listy a =
  Listy [a]
  deriving (Eq, Show)

instance Monoid a => Monoid (Listy a) where
  mempty = Listy []

instance Semigroup a => Semigroup (Listy a) where
  (Listy l) <> (Listy l') =
    Listy $ mappend l l'
