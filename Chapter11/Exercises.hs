{-# LANGUAGE FlexibleInstances #-}
-- | Exercises.hs

module Exercises where

import Data.Int

data Doggies a =
  Husky a
  | Mastiff a
  deriving (Eq, Show)

data DogueDeBordeaux doge =
  DogueDeBordeaux doge

data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer =
  Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
  PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
  Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x y) = x

class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (x, y) = x > 43 && y == "a lot"

instance TooMany (Int, Int) where
  tooMany (x, y) = (x + y) > 43

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = tooMany n || tooMany m

data BigSmall =
  Big Bool
  | Small Bool
  deriving (Eq, Show)

data NumberOrBool =
  Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
  deriving (Eq, Show)

data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
  Haskell
  | Agda
  | Idris
  | Purescript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, Purescript]

allProgrammers :: [Programmer]
allProgrammers = map (\(x, y) -> Programmer x y) [ (x,y) | x<-allOperatingSystems, y<-allLanguages ]
