-- | MonoidExercises

module Chapter15.MonoidExercises where

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity x') = Identity $ x <> x'

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

newtype BoolConj = BoolConj Bool

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

newtype BoolDisj = BoolDisj Bool

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine f') = Combine (\n -> (f n) <> (f' n))

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ (\_ -> mempty)

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (Comp f) <> (Comp f') = Comp (\n -> f $ f' n)

instance Monoid (Comp a) where
  mempty = Comp id

newtype Mem s a =
  Mem { runMem :: s -> (a, s) }

instance (Semigroup a) => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem $ (\s -> (fst (f s) <> fst (g s), snd $ f $ snd $ g s))

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ (\s -> (mempty::(Monoid a) => a, id s))

f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
