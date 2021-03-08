-- | Misc

module Chapter09.Misc where

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

eft :: (Enum a, Ord a) => a -> a -> [a]
eft a b
  | a > b = []
  | a == b = [b]
  | otherwise = a : eft (succ a) b

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft
