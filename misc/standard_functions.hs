-- | My version of standard functions

module StandardFunctions where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny pred (x:xs) = pred x || myAny pred xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem elem (x:xs) = elem == x || myElem elem xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' elem list = myAny (\x -> x == elem) list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy compare list = go compare (head list) (tail list)
  where go f x xs
          | length xs == 0 = x
          | f x (head xs) == LT = go f (head xs) (tail xs)
          | otherwise = go f x (tail xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy compare list = go compare (head list) (tail list)
  where go f x xs
          | length xs == 0 = x
          | f (head xs) x == LT = go f (head xs) (tail xs)
          | otherwise = go f x (tail xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f xs =
  foldr (\x b -> f x || b) False xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

reverse' :: [a] -> [a]
reverse' ss = foldl (flip (:)) [] ss
