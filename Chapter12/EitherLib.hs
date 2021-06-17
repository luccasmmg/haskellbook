-- | EitherLib.hs

module Chapter12.EitherLib where

lefts' :: [Either a b] -> [a]
lefts' xs = foldr (\x y -> y ++ (case x of
                     Left a -> [a]
                     Right _ -> [])) [] xs

rights' :: [Either a b] -> [b]
rights' xs = foldr (\x y -> y ++ (case x of
                     Left _ -> []
                     Right b -> [b])) [] xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f' (Right b) = f' b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\_ -> Nothing) (\y -> Just $ f y) x
