-- | FoldableInstances.hs

module Chapter20.FoldableInstances where

data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f xs = foldMap (\x -> if f x then pure x else mempty) xs
