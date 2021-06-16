-- | Primes.hs

module Primes where

sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]
primes = sieve [2..]
