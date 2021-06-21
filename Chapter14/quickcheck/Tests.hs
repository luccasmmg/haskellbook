-- | Tests.hs

module Tests where

import Test.QuickCheck
import Data.List (sort)
import Lib (Fool(..), half, listOrdered, twice, fourTimes, capitalizeWord)

prop_HalfIdentity :: Float -> Bool
prop_HalfIdentity x = (half x) * 2 == x

prop_ListOrdered :: [Int] -> Bool
prop_ListOrdered x = listOrdered $ sort x

prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative x y = x + y == y + x

prop_plusAssociative' :: Int -> Int -> Int -> Bool
prop_plusAssociative' x y z = x * (y * z) == (x * y) * z

prop_plusCommutative' :: Int -> Int -> Bool
prop_plusCommutative' x y = x * y == y * x

prop_Reverse :: [Int] -> Bool
prop_Reverse x = (reverse $ reverse x) == x

noZero :: Gen (Int, Int)
noZero = do
  a <- getPositive <$> arbitrary
  b <- getPositive <$> arbitrary
  return (a, b)

prop_QuotRem :: Property
prop_QuotRem = forAll noZero (\(x, y) -> (quot x y)*y + (rem x y) == x)

prop_Show :: [Char] -> Bool
prop_Show x = (read (show x)) == x

main :: IO ()
main = do
  quickCheck prop_HalfIdentity
  quickCheck prop_ListOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_plusAssociative'
  quickCheck prop_plusCommutative'
  quickCheck prop_QuotRem
  quickCheck prop_Reverse
  quickCheck prop_Show
