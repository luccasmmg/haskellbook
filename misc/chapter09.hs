-- | Chapter 9 Exercises
module Chapter09 where

import Data.Char

capitalize :: String -> String
capitalize "" = ""
capitalize (s:ss) = toUpper s : capitalize ss

onlyTheFirst :: String -> Char
onlyTheFirst = (head . capitalize)
