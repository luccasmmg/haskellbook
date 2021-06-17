-- | Anamorphism

module Chapter12.Anamorphism where

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x] ++ myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case y of
                  Nothing -> []
                  Just (a, b) -> [a] ++ myUnfoldr f b
                  where
                    y = f x

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just(b, f b)) x

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case y of
               Nothing -> Leaf
               Just (a, b, c) -> (Node (unfold f a) b (unfold f c))
               where y = f x

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold treeBuilder 0
  where
    treeBuilder a
      | a < n     = Just (a + 1, a, a + 1)
      | otherwise = Nothing
