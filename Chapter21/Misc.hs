-- | Misc.hs

module Chapter21.Misc where

myData = [2,4,6,8]

isEvenF x
    | x `rem` 2 == 0 = Just x
    | otherwise      = Nothing

myResult = traverse isEvenF myData
