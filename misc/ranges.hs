-- | Ranges

module Range where

eftBool :: Bool -> Bool -> [Bool]
eftBool False True  = [False, True]
eftBool False False = [False]
eftBool True True   = [True]
eftBool x y         = []

eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd x y
  | x == y = [x]
  | otherwise = x : eftOrd (succ x) y

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x == y = [x]
  | otherwise = x : eftInt (succ x) y

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x == y = [x]
  | otherwise = x : eftChar (succ x) y

breakString :: String -> Char -> [String]
breakString x separator
  | dropWhile (/=separator) x == "" = [x]
  | otherwise = takeWhile (/=separator) x : breakString (tail $ dropWhile (/=separator) x) separator

firstSen = "1 1 1 1 1 1 1 \n"
secondSen = "2 2 2 2 2 2 2 \n"
thirdSen = "3 3 3 3 3 3 3 \n"
fourthSen = "4 4 4 4 4 4 4 \n"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

acro xs = [ x | x <- xs, elem x ['A'..'Z'] ]
