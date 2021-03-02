-- | arith4.hs

module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = (( read :: ( String -> a )  ) ) . ( show :: (b -> String) )

main = do
  print (roundTrip 4)
  print (id 4)
