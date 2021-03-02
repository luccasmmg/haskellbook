-- | Matching Tuples

module TupleFunctions where

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c,f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"
