mult1      = x * y
   where x = 5
         y = 6

mult2 = x * 3 + y
  where x = 3
        y = 1000

mult3 = x * 5
  where y = 10
        x = 10 * 5 + y

mult4 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

data Mood = Blah | Woot deriving Show

changeMood Blah = Woot
changeMood _ = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else x * (-1)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

f' :: (a, b) -> (c, d) -> ((b, d), (a, c))
f' (a, b) (c, d) = ((b, d), (a, c))

addStuff :: Integer -> Integer -> Integer
addStuff = a + b 5
