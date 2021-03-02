-- | Greet If Cool

module GreetIfCool where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True ->
      putStrLn "eyyyyyy whats shakin"
    False ->
      putStrLn
