{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO (readFile)
import Data.Time (getCurrentTime)
import Data.Csv
import Data.List
import Text.ParserCombinators.Parsec

printTime = do
  time <- getCurrentTime
  putStrLn (show time)

printNumbers = do
  putStrLn (show (3+4)) 

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

main :: IO ()
main = do
  putStrLn "hello world"

greet name = "Hello " ++ name ++ "!"

fidelityCSV = "2018-05-16-fidelity.csv"

f::Int -> Maybe Int
f 0 = Nothing
f x = Just x

g :: Int -> Maybe Int
g 100 = Nothing
g x = Just x

j :: Int -> Maybe Int
j 1000 = Nothing
j x = Just x

h ::Int -> Maybe Int
h x = case f x of
        Just n -> g n
        Nothing -> Nothing

h' :: Int -> Maybe Int
h' x = do n <- f x
          m <- g n
          j m

h'' :: Int -> Maybe Int
h'' y = f y >>= g >>= j

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving Show

data Date =
  Date DayOfWeek Int deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday' &&
    dayOfMonth == dayOfMonth'

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn n) (TisAn n') = n == n'

data StringOrInt =
  TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAString x) (TisAString x') = x == x'
  (==) (TisAnInt x) (TisAnInt x') = x == x'
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

data EitherOr a b =
  Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello v) (Hello v') = v == v'
  (==) (Goodbye v) (Goodbye v') = v == v'
  (==) _ _ = False
  
p :: RealFrac a => a
p = 5 / 3

myX = 1
sigmund :: Num a => a -> a
sigmund x = x + (fromInteger myX)

jung :: Ord a => [a] -> a
--jung :: [Int] -> Int
jung xs = head (sort xs)

mySort :: Ord a => [a] -> [a]
mySort = sort

--signifier :: [Char] -> Char
signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f' x y = f' x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f' i x = f' x + (fromInteger i)
