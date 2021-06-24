module ListyInstances where

import Data.Monoid
import Listy

instance Monoid a => Monoid (Listy a) where
  mempty = Listy []

instance Semigroup a => Semigroup (Listy a) where
  (Listy l) <> (Listy l') =
    Listy $ mappend l l'
