module Main (main) where

import System.Environment ( getArgs )
import Data.Bifunctor (first)
import qualified Data.Set as Set

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
solveDay8Parts i = (solveDay8Part1 i, solveDay8Part2 i)

solveDay8Part1 :: Day8Input -> Int
solveDay8Part1 = countVisibleTrees . identifyTrees

countVisibleTrees :: ForestWithIds -> Int
countVisibleTrees = length . getVisibleTrees 

identifyTrees :: Day8Input -> ForestWithIds
identifyTrees (Day8Input i) = ForestWithIds $ identifyTreesWorker i (TreeId 0)
    where identifyTreesWorker [] _ = []
          identifyTreesWorker (l:ls) id = (\(identifiedLine, firstIdInNextLine) -> identifiedLine:identifyTreesWorker ls firstIdInNextLine) $ identifyTreesInLine l id
          identifyTreesInLine [] id = ([], id)
          identifyTreesInLine (t:ts) id = Data.Bifunctor.first (TreeWithId (id, t) :) $ identifyTreesInLine ts (nextTreeId id)
          

getVisibleTrees :: ForestWithIds -> Set.Set TreeId
getVisibleTrees s = getVisibleTreesFromSide firstSide
                     $ getVisibleTreesFromSide secondSide
                     $ getVisibleTreesFromSide thirdSide
                     $ getVisibleTreesFromSide fourthSide Set.empty
                     where firstSide = s
                           secondSide = rotateForest firstSide
                           thirdSide = rotateForest secondSide
                           fourthSide = rotateForest thirdSide

rotateForest :: ForestWithIds -> ForestWithIds
rotateForest (ForestWithIds l) = ForestWithIds $ rotateRectangularList l
    where rotateRectangularList [l] = reverse $ map return l
          rotateRectangularList (l:ls) = addColumnL (reverse l) (rotateRectangularList ls)
          addColumnL (i:is) (l:ls) = (i:l):addColumnL is ls
          addColumnL [] [] = []

getVisibleTreesFromSide :: ForestWithIds -> Set.Set TreeId -> Set.Set TreeId
getVisibleTreesFromSide (ForestWithIds ls) ids = foldl getVisibleTreesInLine ids ls


getVisibleTreesInLine :: Set.Set TreeId -> [TreeWithId] -> Set.Set TreeId
getVisibleTreesInLine ids = fst . foldl addIfHigher (ids, Nothing)
    where addIfHigher (oldSet, Nothing) (TreeWithId (id, height)) = (Set.insert id oldSet, Just height)
          addIfHigher (oldSet, Just oldHeight) (TreeWithId (id, height)) = 
            if height > oldHeight then (Set.insert id oldSet, Just height) else (oldSet, Just oldHeight)

newtype TreeId = TreeId Int deriving (Ord, Eq)

nextTreeId :: TreeId -> TreeId
nextTreeId (TreeId i) = TreeId (i+1)

newtype TreeWithId = TreeWithId (TreeId, Height)
newtype ForestWithIds = ForestWithIds [[TreeWithId]]


solveDay8Part2 :: Day8Input -> Int
solveDay8Part2 x = undefined