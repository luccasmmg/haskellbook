-- | Exercises in case expression

module CaseExpression where

functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n =
  case even n of
    True -> (n + 2)
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \ x y -> f y x
