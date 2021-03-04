-- | Does it type check

module Chapter06.DoesItTypeCheck where

data Person = Person Bool deriving Show

printPerson :: Person -> IO()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot deriving (Eq, Show)

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
s2 = Sentence "dogs" "drool" "dogs"

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith aToB i a = aToB a
