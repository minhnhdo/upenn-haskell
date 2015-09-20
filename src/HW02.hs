module HW02 where

import Data.Function (on)

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = length . filter (uncurry (==)) $ zip a b

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors ps = map (\x -> length $ filter (== x) ps) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum . map (uncurry min) $ (zip `on` countColors) xs ys

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess em (matches secret guess - em)
  where em = exactMatches secret guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move m em nem) guess = em == em' && nem == nem'
  where Move _ em' nem' = getMove m guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
  | n == 0    = []
  | n == 1    = map (: []) colors
  | otherwise = [peg : code | code <- allCodes (n - 1), peg <- colors]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = go (allCodes $ length secret) []
  where go [] acc = reverse acc
        go (g:guesses) acc = let move = getMove secret g
                             in go (filterCodes move guesses)  (move:acc)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
