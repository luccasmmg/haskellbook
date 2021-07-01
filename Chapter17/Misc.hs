-- | Misc.hs

module Chapter17.Misc where

import Control.Applicative

f x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbat")]

g y = lookup y [ (7, "sup")
               , (8, "chris")
               , (9, "aloha")]

h z = lookup z [(2,3), (5,6), (7,8)]
m x = lookup x [(4,10), (8,13), (1,9001)]

data Cow = Cow {
    name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' = Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'
