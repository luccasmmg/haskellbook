-- | FixerUpper.hs

module Chapter17.FixerUpper where

y = const <$> Just "Hello" <*> pure "World"

x = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
