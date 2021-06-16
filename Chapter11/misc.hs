-- | misc.hs

module Misc where

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

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType =
  DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving Show

data Farmer =
  Farmer Name Acres FarmerType
  deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False
