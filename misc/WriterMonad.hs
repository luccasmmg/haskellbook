-- | WriterMonad.hs

module WriterMonad where

import Data.Monoid
import Data.Foldable
import Control.Monad.Writer
import Control.Monad.State

fact1 :: Integer -> Writer String Integer
fact1 0 = return 1
fact1 n = do
  let n' = n-1
  tell $ "We've taken one away from " ++ show n ++ "\n"
  m <- fact1 n'
  tell $ "We've called f " ++ show m ++ "\n"
  let r = n*m
  tell $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
  return r

fact2 :: Integer -> Writer (Sum Integer) Integer
fact2 0 = return 1
fact2 n = do
  let n' = n-1
  tell $ Sum 1
  m <- fact2 n'
  let r = n*m
  tell $ Sum 1
  return r
