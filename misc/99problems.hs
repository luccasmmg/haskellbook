-- | Trying to solve the 99 Problems in Haskell

module NineNineProblems where

myLast :: [a] -> a
myLast [] = error "no end for empty lists"
myLast x =  x !! (length x - 1)

myButLast :: [a] -> a
myButLast [] = error "no end for empty lists"
myButLast [x] = error "Only one element"
myButLast x =  x !! (length x - 2)

elementAt :: [a] -> Int -> a
elementAt [] _ = error "No such element"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n - 1)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x
