-- | Optional.hs

module Chapter15.Optional where

import Data.Monoid

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  (Only x) <> (Only y) = Only $ x <> y
  (Only x) <> Nada = Only $ x
  Nada <> (Only y) = Only $ y
  Nada <> Nada = Nada
