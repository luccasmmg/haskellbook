-- | ChapterExercises.hs

module Chapter21.ChapterExercises where

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldr f z (Identity b) = f b z

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

instance Foldable (Constant m) where
  foldr _ z (Constant _) = z

instance Traversable (Constant m) where
  traverse _ (Constant v) = pure (Constant v)

data Optional a = Nada | Yep a

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep a) = f a z

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x y) = (Cons (f x) (fmap f y))

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x y) = f x <> foldMap f y

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x y) = Cons <$> f x <*> traverse f y

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> (f c)

data Pair a b = Pair a b

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldr f z (Pair _ b) = f b z

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> (f b)

data Big a b = Big a b b

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> (f b) <*> (f b')

data Bigger a b = Bigger a b b b

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''
