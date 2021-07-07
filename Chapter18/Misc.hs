-- | Misc.hs

module Chapter18.Misc where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap (\x -> f x)

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >> getLine >>= \name -> putStrLn("y helo thar: " ++ name)

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = xs >>= \x -> (tail xs) >>= \x' -> if x == x' then [x*x, x*x] else [x*x]

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

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
      in if n == "Bless" && w > 499
            then Nothing
                 else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)
