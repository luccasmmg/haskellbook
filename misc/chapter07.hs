-- | Chapter 07 Exercises

module Chapter07 where

mth x y z = x * y * z
mth' x y = \z -> x * y * z
mth'' x = \y -> \z -> x * y * z
mth''' = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x
