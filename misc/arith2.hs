-- | arith2.hs

module Arith2 where

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO ()
main = do
  print (0 :: Int) -- 0
  print (add 1 0) -- 1
  print (addOne 0) -- 1
  print (addOnePF 0) -- 1
  print ((addOne . addOne) 0) -- 2

  print ((addOnePF . addOne) 0) -- 2
  print ((addOne . addOnePF) 0) -- 2
  print ((addOnePF . addOnePF) 0)
  print (negate (addOne 0)) -- -1
  print ((negate . addOne) 0) -- -1
  print ((addOne . addOne . addOne
         . negate . addOne) 0) -- 2

tensDigit x = d
  where d = x `divMod` 10

hunsD x = d2
  where d2 = x `divMod` 100

foldBool :: a -> a -> Bool -> a
foldBool x y bool
  | bool == False = x
  | bool == True = y

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool =
  case bool of
    True -> y
    False -> x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
