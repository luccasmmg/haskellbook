-- | Trivial

module Chapter06.Trivial where

data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True
