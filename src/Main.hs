{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO (readFile)
import Data.Time (getCurrentTime)
import Data.Csv
import Data.List
import Text.ParserCombinators.Parsec
import System.Random
import System.IO.Unsafe

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

dupeMe x = [x, x]
listO'Dupes xs = map dupeMe xs
listO'Dupe2 xs = xs >>= dupeMe
listO'Dupe3 xs = (>>=) xs dupeMe
mapcat f xs = xs >>= f

---- Poker

data Suit = Diamonds
          | Clubs
          | Hearts
          | Spades
          deriving (Eq, Show, Enum, Bounded)

instance Ord Suit where
  compare _ _ = EQ

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Eq, Ord, Show, Enum, Bounded)

data Card =
  Card Rank Suit
  deriving (Eq, Ord)

instance Show Card where
  show (Card r s) = show r ++ " of " ++ show s

cardsForRank r = map (\s -> Card r s) [Diamonds ..]

deckO'Cards = [Two ..] >>= cardsForRank

shuffleList :: [a] -> IO [a]
shuffleList [] = return [] -- An empty list cannot be shuffled more
shuffleList [x] = return [x] -- A list with one element cannot be shuffled more
shuffleList as = do
    i <- randomRIO (0,length as-1)
    shuffledRest <- shuffleList (take i as ++ drop (i+1) as)
    return $ (as !! i) : shuffledRest

shuffledDeckOfCards = shuffleList deckO'Cards
unsafeShuffledCardsOnce = unsafePerformIO (shuffleList deckO'Cards)

headNTail :: [a] -> Int -> ([a], a, [a])
headNTail xs n = let (y:ys) = drop n xs
                 in (take n xs, y, ys)

deal :: [a] -> Int -> Int -> ([[a]], [a])
deal shuffledCards numCards numPeople =
  let dealF :: [[a]] -> [a] -> Int -> Int -> ([[a]], [a])
      dealF dealt deck      0  0  = (dealt, deck)
      dealF dealt []        _  _  = dealF dealt [] 0 0
      dealF dealt deck      cn 0  = dealF dealt deck (cn-1) numPeople
      dealF dealt (c1:deck) cn pn =
        let (h, x, t) = headNTail dealt (numPeople - pn)
        in dealF (h ++ [(c1:x)] ++ t) deck cn (pn - 1)
  in dealF (take numPeople $ repeat []) shuffledCards numCards numPeople

data PokerHandRank = HighCard
                   | APair
                   | TwoPair
                   | ThreeOfAKind
                   | Straight
                   | Flush
                   | FullHouse
                   | FourOfAKind
                   | StraightFlush
                   | RoyalFlush
                   deriving (Eq, Ord, Show, Enum, Bounded)

sameSuit :: Card -> Card -> Bool
sameSuit (Card x _) (Card x' _) = x == x'

groupAndSortOfAKind :: [Card] -> [[Card]]
groupAndSortOfAKind cards =  reverse . (sortBy lenSorter) . (groupBy sameSuit) . sort $ cards
  where lenSorter :: [a] -> [a] -> Ordering
        lenSorter l l' = compare (length l) (length l')

handRank :: [Card] -> PokerHandRank
handRank cards
  | isAHighStraight && isAFlush                     = RoyalFlush
  | isAStraight && isAFlush                         = StraightFlush
  | nOfAKind 4 grouped                              = FourOfAKind
  | nOfAKind 3 grouped && nOfAKind 2 (tail grouped) = FullHouse
  | isAFlush                                        = Flush
  | isAStraight                                     = Straight
  | nOfAKind 3 grouped                              = ThreeOfAKind
  | nOfAKind 2 grouped && nOfAKind 2 (tail grouped) = TwoPair
  | nOfAKind 2 grouped                              = APair
  | otherwise                                       = HighCard
  where
    sorted          = sort cards
    ranks           = map (\(Card r _) -> r) sorted
    suits           = map (\(Card _ s) -> s) sorted
    grouped         = groupAndSortOfAKind sorted
    isAFlush        = all (\s -> s == head suits) suits
    isAStraight     = take (length cards) [(head ranks) ..] == ranks
    isAHighStraight = (head ranks == Ten) && isAStraight
    nOfAKind :: Int -> [[Card]] -> Bool
    nOfAKind n groupedCards = (length $ head groupedCards) == n

aRoyalFlush    = [Card Ten Hearts, Card Jack Hearts, Card Queen Hearts, Card King Hearts, Card Ace Hearts]
aStraightFlush = [Card Two Hearts, Card Three Hearts, Card Four Hearts, Card Five Hearts, Card Six Hearts]
aFourOfAKind   = [Card Two Spades, Card Two Hearts, Card Two Diamonds, Card Two Clubs, Card Queen Hearts]
aFullHouse     = [Card Two Spades, Card Two Hearts, Card Two Diamonds, Card King Clubs, Card King Hearts]
aFlush         = [Card Two Spades, Card Four Spades, Card Ace Spades, Card Jack Spades, Card Nine Spades]
aStraight      = [Card Two Spades, Card Three Hearts, Card Four Diamonds, Card Five Clubs, Card Six Hearts]
aThreeOfAKind  = [Card Two Spades, Card Two Hearts, Card Two Diamonds, Card King Clubs, Card Queen Hearts]
aTwoPair       = [Card Two Spades, Card Two Hearts, Card Jack Diamonds, Card Jack Clubs, Card Queen Hearts]
aPair          = [Card Two Spades, Card Two Hearts, Card Jack Diamonds, Card King Clubs, Card Queen Hearts]
aHighCard      = [Card Two Spades, Card Five Hearts, Card Jack Diamonds, Card King Clubs, Card Queen Hearts]

testHandRank :: [Card] -> PokerHandRank -> String
testHandRank hand expectedRank =
  (show expectedRank) ++ ": " ++ (show $ handRank hand == expectedRank)

runTests :: IO ()
runTests = do
  putStrLn "Running tests"
  putStrLn $ testHandRank aRoyalFlush    RoyalFlush
  putStrLn $ testHandRank aStraightFlush StraightFlush
  putStrLn $ testHandRank aFourOfAKind   FourOfAKind
  putStrLn $ testHandRank aFullHouse     FullHouse
  putStrLn $ testHandRank aFlush         Flush
  putStrLn $ testHandRank aStraight      Straight
  putStrLn $ testHandRank aThreeOfAKind  ThreeOfAKind
  putStrLn $ testHandRank aTwoPair       TwoPair
  putStrLn $ testHandRank aPair          APair
  putStrLn $ testHandRank aHighCard      HighCard

-- Example of guard in where clause
divides :: Integer -> Integer -> Bool
divides small big = (big `mod` small == 0)

lowestDivisor :: Integer -> Integer
lowestDivisor n = lowestDivisorHelper 2 n
  where lowestDivisorHelper m n
          | (m `divides` n) = m  -- these should belong to lowestDivisorHelper
          | otherwise = lowestDivisorHelper (m+1) n
