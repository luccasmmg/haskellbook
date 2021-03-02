-- | Chapter 06 Type Checks

module Chapter06 where

data Person = Person Bool  deriving (Show, Eq)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
            | Woot deriving (Show, Eq)

settleDown x = if x == Woot
                  then Blah
                  else x

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

freud :: Ord a => a -> a
freud x = x

arith :: Num b
         => (a -> b)
         -> Integer
         -> a
         -> b
arith aToB int a = (aToB a)
