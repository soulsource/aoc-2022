module Main where

import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= print . solveDay1

solveDay1 :: String -> (Integer, Integer)
solveDay1 x = (solveDay1Part1 x, solveDay1Part2 x)

solveDay1Part1 :: String -> Integer
solveDay1Part1 = maximum . sums

solveDay1Part2 :: String -> Integer
solveDay1Part2 = maybeSum . foldl findTopThree (Nothing, Nothing, Nothing) . sums

sums :: String -> [Integer]
sums = map sum . groups

groups :: String -> [[Integer]]
groups = map (map read) . groupStrings

groupStrings :: String -> [[String]]
groupStrings = splitAtEmpty . lines

splitAtEmpty :: [String] -> [[String]]
splitAtEmpty = foldl concatOrAppend []

concatOrAppend :: [[String]] -> String -> [[String]]
concatOrAppend rs "" = [] : rs
concatOrAppend (r:rs) n = (n : r) : rs
concatOrAppend [] n = [[n]]

findTopThree :: (Maybe Integer, Maybe Integer, Maybe Integer) -> Integer -> (Maybe Integer, Maybe Integer, Maybe Integer)
findTopThree (Nothing,_,_) d = (Just d, Nothing, Nothing)
findTopThree (Just a,b,_) d | d > a = (Just d, Just a, b)
findTopThree (Just a, Just b, _) d | d > b = (Just a, Just d, Just b)
findTopThree (Just a, Just b, Just c) d | d > c = (Just a, Just b, Just d)
findTopThree r _ = r

maybeSum :: (Maybe Integer, Maybe Integer, Maybe Integer) -> Integer
maybeSum (a,b,c) = zeroIfNothing a + zeroIfNothing b + zeroIfNothing c

zeroIfNothing :: Maybe Integer -> Integer
zeroIfNothing (Just a) = a
zeroIfNothing Nothing = 0