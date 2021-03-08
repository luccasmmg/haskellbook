-- | Exercises
module Chapter09.Exercises where

import Data.Bool

separation :: (Eq a) => a -> [a] -> [[a]]
separation s xs
  | dropWhile (/=s) xs == [] = [xs]
  | otherwise = takeWhile (/=s) xs : separation s (tail (dropWhile (/=s) xs))

myWords' = separation ' '
myLines' = separation '\n'

myWords :: [Char] -> [[Char]]
myWords xs
  | dropWhile (/=' ') xs == "" = [xs]
  | otherwise = takeWhile (/=' ') xs : myWords (tail (dropWhile (/=' ') xs))

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith (,)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines xs
  | dropWhile (/='\n') xs == "" = [xs]
  | otherwise = takeWhile (/='\n') xs : myLines (tail (dropWhile (/='\n') xs))
 
shouldEqual = ["Tyger Tyger, burning bright" ,"In the forests of the night" ,"What immortal hand or eye" ,"Could frame thy fearful symmetry?"]
-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

