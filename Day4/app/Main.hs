module Main (main) where

import System.Environment ( getArgs )
import Data.Bifunctor ( Bifunctor(bimap, first) )

main :: IO ()
main = getArgs >>= readFile . head >>= print . solveDay4

solveDay4 :: String -> (Int, Int)
solveDay4 x = (solveDay4Part1 x, solveDay4Part2 x)

solveDay4Part1 :: String -> Int
solveDay4Part1 = length . filter containingRanges . map parseRanges . lines

newtype CleaningCells = CleaningCells (Integer,Integer)

parseRanges :: String -> (CleaningCells, CleaningCells)
parseRanges = Data.Bifunctor.bimap parseRange parseRange . splitAtChar ','

splitAtChar :: Char -> String -> (String, String)
splitAtChar c x = splitAtCharacter c ("",x)

splitAtCharacter :: Char -> (String, String) -> (String, String)
splitAtCharacter c (a, b:bs) = if b == c then (a,bs) else Data.Bifunctor.first (b:) (splitAtCharacter c (a, bs))
splitAtCharacter c (a,[]) = (a,[])

parseRange :: String -> CleaningCells
parseRange = CleaningCells . Data.Bifunctor.bimap read read . splitAtChar '-' 

containingRanges :: (CleaningCells, CleaningCells) -> Bool
containingRanges (CleaningCells (a1,b1), CleaningCells (a2,b2)) = (a1 >= a2 && b1 <= b2) || (a2 >= a1 && b2 <= b1)

solveDay4Part2 :: String -> Int
solveDay4Part2 = length . filter overlappingRanges . map parseRanges . lines

overlappingRanges :: (CleaningCells, CleaningCells) -> Bool
overlappingRanges (CleaningCells (a1,b1), CleaningCells (a2,b2)) = 
    (a1 >= a2 && a1 <= b2) || (b1 >= a2 && b1 <= b2) || (a2 >= a1 && a2 <= b1) || (b2 >= a1 && b2 <= b1)