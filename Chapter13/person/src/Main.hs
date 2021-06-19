module Main where

import Text.Read (readEither)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Give me a name for the person: "
  name <- getLine
  putStrLn "Give me a age for the person"
  age <- getLine
  case readEither age of
    Right age' -> case (mkPerson name age') of
                      Right person -> putStrLn $ show person
                      Left personInvalid -> putStrLn $ "A error ocorred: " ++ show personInvalid
    Left error' -> putStrLn $ "A error ocorred: " ++ error'

main :: IO ()
main = do
  gimmePerson
