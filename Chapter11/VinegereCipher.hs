-- | VinegereCipher

module Chapter11.VinegereCipher where

import Data.Char

generateAllyString :: [Char] -> [(Char, Char)]
generateAllyString word = go word (take (length word) (cycle "ally"))
  where
    go [] _ = []
    go (' ':xs) (y:ys) = (' ', ' ') : go xs (y:ys)
    go (x:xs) (y:ys) = (if x == ' ' then (' ', ' ') else (x, y)) : go xs ys

convertOneTuple :: (Char, Char) -> Char
convertOneTuple (x, y)
  | x == ' ' = ' '
  | ord x + n < 123 = (chr $ (mod (ord x) 122) + n)
  | otherwise = (chr $ (mod ((ord x) + n) 123) + 97)
  where n = ord y - 97

decipherOneTuple :: (Char, Char) -> Char
decipherOneTuple (x, y)
  | x == ' ' = ' '
  | ord x - n > 96 = (chr $ (mod (ord x) 123) - n)
  | otherwise = (chr $ mod ((ord x) - (ord y) + 123) 122)
  where n = ord y - 97

vinegere :: [Char] -> [Char]
vinegere word = map convertOneTuple (generateAllyString word)

unVinegere :: [Char] -> [Char]
unVinegere word = map decipherOneTuple (generateAllyString word)

ciphered = vinegere "meet at dawn"
deciphered = unVinegere (vinegere "meet at dawn")
