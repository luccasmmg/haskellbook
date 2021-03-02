sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello " ++ x ++ "!")

mult1 = x * y
  where x = 5
        y = 6

timesThreePlusY = x * 3 + y
  where x = 3
        y = 1000

times5 = x * 5
  where x = 10 * 5 + y
        y = 10

zDividedByXPlusY = z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn = x * 5
  where z = 7
        y = 7 + 8
        x = y ^ 2

triple x = x * 3

waxOff x = triple x

area d = pi * (r * r)
  where r = d / 2

addLast x = x ++ "!"

getFourth x = head $ drop 4 x

getAfterNinth x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = head $ drop 3 x

letterIndex :: Int -> Char
letterIndex x = head $ drop x "Curry is awesome!"

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse' x == x

test :: Integer -> Integer
test x = (-1) * x

myAbs :: Integer -> Integer
myAbs x =
  if x > 0
     then x
  else
    x * (-1)

f xs = x w 1
  where x = (+)
        w = length xs

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

substractStuff :: Integer
               -> Integer
               -> Integer
substractStuff x y = x - y - 10
subtractOne = substractStuff 1

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer
                -> Bool
                -> Integer
curriedFunction i b =
  i + (nonsense b)

uncurriedFunction :: (Integer, Bool)
                   -> Integer
uncurriedFunction (i, b) =
  i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer
           -> Bool
           -> Integer
anonNested =
  \i -> \b -> i + (nonsense b)

test'' :: a -> a -> a
test'' x y = id x

f :: (a, b) -> (c, d) -> ((b, d). (a, c))
f (a, b) (c, d) = ((b, d), (a, c))
