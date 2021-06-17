-- | NaturalNumbers.hs

module Chapter12.NaturalNumbers where

data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just $ go x
  where
    go 0 = Zero
    go x' = Succ $ go (x' - 1)
