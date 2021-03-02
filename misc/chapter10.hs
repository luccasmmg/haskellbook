-- | Data Structure Origami

module Chapter10 where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello World!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem]
              -> [UTCTime]
filterDbDate db = [ item | DbDate item <- db ]

filterDbNumber :: [DatabaseItem]
              -> [Integer]
filterDbNumber db = [ item | DbNumber item <- db ]

mostRecent :: [DatabaseItem]
           -> UTCTime
mostRecent db = foldr (\x y -> if x < y then x else y) (UTCTime
                                                       (fromGregorian 3000 5 1)
                                                       (secondsToDiffTime 0)) (filterDbDate db)

sumDb :: [DatabaseItem]
      -> Integer
sumDb db = foldr (+) 0 $ filterDbNumber db

avgDb :: [DatabaseItem]
      -> Double
avgDb db = (fromIntegral $ sumDb db) / (fromIntegral $ length $ filterDbNumber db)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f =
  foldr (\x b -> f x || b) False

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x y -> if f x then x : y else y) []

squish' :: [[a]] -> [a]
squish' = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x y -> f x ++ y) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy f (x:xs) = foldr (\x y -> if f x y == GT then x else y)  

myMinimumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMinimumBy f (x:xs) = foldr (\x y -> if f x y == LT then x else y)
