-- | StringProcessing.hs

module Chapter12.StringProcessing where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str = Just str

replaceThe :: String -> String
replaceThe = foldr (++) "" . map (\x -> case (notThe x) of
                                     Nothing -> " a"
                                     Just a -> " " ++ a) . words

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = go $ words xs
  where
    go [] = 0
    go [_] = 0
    go (x:x':xs') =
          (if x == "the" && elem (head x') "aeiou" then 1 else 0) + go (x':xs')

countTheBeforeVowel' :: String -> [String]
countTheBeforeVowel' xs = go $ words xs
  where
    go [] = [""]
    go [_] = [""]
    go (x:x':xs') = x':xs'

countVowels :: String -> Integer
countVowels xs = fromIntegral . length $ filter (\x -> elem x "aeiou") xs

countConsonants :: String -> Integer
countConsonants xs = fromIntegral . length $ filter (\x -> not $ elem x "aeiou") xs

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str = case (countVowels str) < (countConsonants str) of
  True -> Just $ Word' str
  False -> Nothing

data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x
