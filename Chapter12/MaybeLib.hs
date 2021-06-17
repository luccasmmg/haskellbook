-- | MaybeLib.hs

module Chapter12.MaybeLib where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing x = not $ isJust x

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x
mayybee base _ Nothing = base

fromMaybe :: a -> Maybe a -> a
fromMaybe base Nothing = mayybee base id Nothing
fromMaybe base x = mayybee base id x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes xs = concat $ map maybeToList $ filter (\x -> isJust x) xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:_) = Nothing
flipMaybe (Just x:xs)  = case flipMaybe xs of Nothing -> Nothing
                                              Just l  -> Just (x:l)
