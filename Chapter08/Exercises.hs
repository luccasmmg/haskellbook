-- | Exercises

module Chapter08.Exercises where

dividedBy :: Integer -> Integer -> (Integer, Integer)
dividedBy x y = go x y 0
  where go x' y' count
          | x' < y' = (x', count)
          | otherwise =
            go (x' - y') y' (count + 1)

sumAll :: (Eq a, Num a) => a -> a
sumAll 0 = 0
sumAll x = x + sumAll (x - 1)

multiplies :: (Integral a) => a -> a -> a
multiplies x 1 = x
multiplies x y = x + multiplies x (y - 1)

data DividedResult =
  Result (Integer, Integer)
  | DividedByZero deriving Show

dividedBy' :: Integer -> Integer -> DividedResult
dividedBy' x 0 = DividedByZero
dividedBy' x y = go x y 0
  where go x' y' count
          | (abs x') < (abs y') = Result (x', count)
          | (x' < 0) /= (y' < 0) = go (x' + y') y' (count - 1)
          | (x' < 0) == (y' < 0) = go (x' - y') y' (count + 1)

mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ (x + 11)
