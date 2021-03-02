-- | Recursion

module Chapter08 where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

applyTimes :: (Eq a, Num a) =>
              a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

applyTimes' :: (Eq a, Num a) =>
              a -> (b -> b) -> b -> b
applyTimes' 0 f b = b
applyTimes' n f b = f . applyTimes (n-1) f $ b

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

sumOfRange :: (Eq a, Num a) => a -> a
sumOfRange 0 = 0
sumOfRange n = n + sumOfRange (n - 1)

multipliedBy :: (Integral a) => a -> a -> a
multipliedBy x 1 = x
multipliedBy x y = go x 0 y
  where go x' total count
          | count == 0 = total
          | otherwise =
            go x' (total + x) (count - 1)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | abs n < abs d = (count, n)
          | n > 0 && d > 0 || n < 0 && d < 0 =
              go (n - d) d (count + 1)
          | otherwise =
              go (n + d) d (count - 1)

mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | otherwise =
      mc91 (mc91 (n + 11))
