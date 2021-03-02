-- | Lists - Chapter 09

module Lists where

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
