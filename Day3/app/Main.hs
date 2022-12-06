module Main (main) where

import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= print . solveDay3

solveDay3 :: String -> (Integer, Integer)
solveDay3 x = (solveDay3Part1 x, solveDay3Part2 x)

solveDay3Part1 :: String -> Integer
solveDay3Part1 = sum . map getLinePriority . lines

getLinePriority :: String -> Integer
getLinePriority = getCharPriority . findCharacterInFirstAndSecondHalf

getCharPriority :: Maybe Char -> Integer
getCharPriority (Just 'a') = 01
getCharPriority (Just 'b') = 02
getCharPriority (Just 'c') = 03
getCharPriority (Just 'd') = 04
getCharPriority (Just 'e') = 05
getCharPriority (Just 'f') = 06
getCharPriority (Just 'g') = 07
getCharPriority (Just 'h') = 08
getCharPriority (Just 'i') = 09
getCharPriority (Just 'j') = 10
getCharPriority (Just 'k') = 11
getCharPriority (Just 'l') = 12
getCharPriority (Just 'm') = 13
getCharPriority (Just 'n') = 14
getCharPriority (Just 'o') = 15
getCharPriority (Just 'p') = 16
getCharPriority (Just 'q') = 17
getCharPriority (Just 'r') = 18
getCharPriority (Just 's') = 19
getCharPriority (Just 't') = 20
getCharPriority (Just 'u') = 21
getCharPriority (Just 'v') = 22
getCharPriority (Just 'w') = 23
getCharPriority (Just 'x') = 24
getCharPriority (Just 'y') = 25
getCharPriority (Just 'z') = 26
getCharPriority (Just 'A') = 27
getCharPriority (Just 'B') = 28
getCharPriority (Just 'C') = 29
getCharPriority (Just 'D') = 30
getCharPriority (Just 'E') = 31
getCharPriority (Just 'F') = 32
getCharPriority (Just 'G') = 33
getCharPriority (Just 'H') = 34
getCharPriority (Just 'I') = 35
getCharPriority (Just 'J') = 36
getCharPriority (Just 'K') = 37
getCharPriority (Just 'L') = 38
getCharPriority (Just 'M') = 39
getCharPriority (Just 'N') = 40
getCharPriority (Just 'O') = 41
getCharPriority (Just 'P') = 42
getCharPriority (Just 'Q') = 43
getCharPriority (Just 'R') = 44
getCharPriority (Just 'S') = 45
getCharPriority (Just 'T') = 46
getCharPriority (Just 'U') = 47
getCharPriority (Just 'V') = 48
getCharPriority (Just 'W') = 49
getCharPriority (Just 'X') = 50
getCharPriority (Just 'Y') = 51
getCharPriority (Just 'Z') = 52
getCharPriority _ = 0

findCharacterInFirstAndSecondHalf :: String -> Maybe Char
findCharacterInFirstAndSecondHalf x =  findElementInBothLists (splitAt (length x `div` 2) x)

findElementInBothLists :: (Eq a) => ([a],[a]) -> Maybe a
findElementInBothLists (a:as,b) = if a `elem` b then Just a else findElementInBothLists (as,b)
findElementInBothLists _ = Nothing

-- Todo
solveDay3Part2 :: String -> Integer
solveDay3Part2 = sum . map getGroupPriority . elfGroups

getGroupPriority :: (String, String, String) -> Integer
getGroupPriority = getCharPriority . findElementInThreeLists 

findElementInThreeLists :: (Eq a) => ([a],[a],[a]) -> Maybe a
findElementInThreeLists (a:as,b,c) = if a `elem` b && a `elem` c then Just a else findElementInThreeLists (as,b,c)

elfGroups :: String -> [(String, String, String)]
elfGroups = groupThree . lines

groupThree :: [a] -> [(a,a,a)]
groupThree (a:b:c:as) = (a,b,c) : groupThree as
groupThree _ = []