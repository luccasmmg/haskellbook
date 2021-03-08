-- | Chapter Exercises

module Chapter09.Exercises_2 where

import Data.Char

justUpperCase :: [Char] -> [Char]
justUpperCase = filter isUpper

toUpperFirst :: [Char] -> [Char]
toUpperFirst [] = []
toUpperFirst (x:xs) = toUpper x : xs

toUpperFull :: [Char] -> [Char]
toUpperFull [] = []
toUpperFull (x:xs) = toUpper x : toUpperFull xs

myOr :: [Bool] -> Bool
myOr [] = True
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
