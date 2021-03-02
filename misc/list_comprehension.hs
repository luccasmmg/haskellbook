-- | List Comprehension

module ListComprehension where

acro xs = [ x | x <- xs, elem x ['A'..'Z'] ]

myFilter :: String -> [String]
myFilter sentence = filter
                    (\x -> all (/=x) ["the", "a", "an"])
                    $ words sentence
