-- | AsPattern.hs

module Chapter11.AsPattern where

import qualified Data.Char as Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs'@(x:xs) ys'@(y:ys) = if x `elem` ys'
    then isSubseqOf xs ys
    else isSubseqOf xs' ys


capitalizeWord :: String -> (String, String)
capitalizeWord xs'@(x:xs) = (xs', Char.toUpper x : xs)

capitalizeWord' :: String -> String
capitalizeWord' (x:xs) = Char.toUpper x : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords word = map capitalizeWord (words word)
