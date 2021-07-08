-- | LibraryFunctions.hs

module Chapter20.LibraryFunctions where

import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e xs = foldr (\x _ -> if x == e then True else False) False xs

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr fmin Nothing
  where fmin x Nothing  = Just x
        fmin x (Just y) = Just (min x y)

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr fmax Nothing
  where fmax x Nothing  = Just x
        fmax x (Just y) = Just (max x y)

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ y -> y + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (\x y -> x : y) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr (\x y -> f x <> y) mempty xs
