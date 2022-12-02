module Main (main) where

import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= print . solveDay2

solveDay2 :: String -> (Integer, Integer)
solveDay2 x = (solveDay2Part toGamePart1 x, solveDay2Part toGamePart2 x)

solveDay2Part :: (String -> Game) -> String -> Integer
solveDay2Part c = sum . map (pointsFromString c) . lines

data Sign = Rock | Paper | Scissors deriving Eq
newtype Game = Game (Sign, Sign)

pointsFromString :: (String -> Game) -> String -> Integer
pointsFromString = (points .)

points :: Game -> Integer
points (Game (opponent, mine)) = winning opponent mine + cardScore mine
  where winning a b | a == b = 3
        winning Rock Paper = 6
        winning Rock Scissors = 0
        winning Paper Scissors = 6
        winning Paper Rock = 0
        winning Scissors Rock = 6
        winning Scissors Paper = 0
        cardScore Rock = 1
        cardScore Paper = 2
        cardScore Scissors = 3

toGamePart1 :: String -> Game
toGamePart1 [a,_,b] = Game (toSign a, toSign b)

toGamePart2 :: String -> Game
toGamePart2 [a,_,b] = Game (toSign a, toCheatedSign (toSign a) (toCheat b))

toSign :: Char -> Sign
toSign 'A' = Rock
toSign 'B' = Paper
toSign 'C' = Scissors
toSign 'X' = Rock
toSign 'Y' = Paper
toSign 'Z' = Scissors

data Cheat = Win | Draw | Lose

toCheatedSign :: Sign -> Cheat -> Sign
toCheatedSign a Draw = a
toCheatedSign Rock Win = Paper
toCheatedSign Rock Lose = Scissors
toCheatedSign Paper Win = Scissors
toCheatedSign Paper Lose = Rock
toCheatedSign Scissors Win = Rock
toCheatedSign Scissors Lose = Paper

toCheat :: Char -> Cheat
toCheat 'X' = Lose
toCheat 'Y' = Draw
toCheat 'Z' = Win