module Main (main) where

import System.Environment ( getArgs )

main :: IO ()
main = getArgs >>= readFile . head >>= print . solveDay8

solveDay8 :: String -> String
solveDay8 = formatDay8Result . fmap solveDay8Parts . parseDay8Input

formatDay8Result :: Maybe (Int, Int) -> String
formatDay8Result Nothing = "Input didn't parse. Check that it is a rectangular grid with values from 0-9."
formatDay8Result (Just (p1, p2)) = "Part 1: " ++ show p1 ++ ", Part 2: " ++ show p2

newtype Day8Input = Day8Input [[Height]] -- but with guarantee that it's rectangular

data Height = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    deriving (Eq, Ord)

heightFromChar :: Char -> Maybe Height -- this could also be done using the Enum typeclass' toEnum function and an intermediate Int
heightFromChar '0' = Just Zero
heightFromChar '1' = Just One
heightFromChar '2' = Just Two
heightFromChar '3' = Just Three
heightFromChar '4' = Just Four
heightFromChar '5' = Just Five
heightFromChar '6' = Just Six
heightFromChar '7' = Just Seven
heightFromChar '8' = Just Eight
heightFromChar '9' = Just Nine
heightFromChar _ = Nothing

validateDay8Input :: [[Height]] -> Maybe Day8Input
validateDay8Input [] = Nothing
validateDay8Input (h:hs) = if all ((== length h) . length) hs then Just $ Day8Input $ h:hs else Nothing

parseDay8Input :: String -> Maybe Day8Input
parseDay8Input = (=<<) validateDay8Input . mapM (mapM heightFromChar) . lines

solveDay8Parts :: Day8Input -> (Int, Int)
solveDay8Parts = undefined