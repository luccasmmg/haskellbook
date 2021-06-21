-- | IdentityCrisis.hs

module Chapter14.IdentityCrisies where

data Identity a =
  Identity a
  deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)
