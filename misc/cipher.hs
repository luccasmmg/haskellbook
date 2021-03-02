-- | Cipher module

module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar _ "" = ""
caesar n (s:ss)
  | ord s + n < 123 = (chr $ (mod (ord s) 122) + n) : caesar n ss
  | otherwise = (chr $ (mod ((ord s) + n) 123) + 97) : caesar n ss

unCaesar :: Int -> String -> String
unCaesar _ "" = ""
unCaesar n (s:ss)
  | ord s - n > 96 = (chr $ (mod (ord s) 123) - n) : unCaesar n ss
  | otherwise = (chr $ (mod 121 (ord s)) + 97) : unCaesar n ss
