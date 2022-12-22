module Main (main) where

import System.Environment ( getArgs )
import Data.Bifunctor (first)
import Data.List (transpose)
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
                           secondSide = rotateForestLeft firstSide
                           thirdSide = rotateForestLeft secondSide
                           fourthSide = rotateForestLeft thirdSide

rotateForestLeft :: ForestWithIds -> ForestWithIds
rotateForestLeft (ForestWithIds l) = ForestWithIds $ rotateRectangularListLeft l

rotateRectangularListLeft :: [[a]] -> [[a]]
rotateRectangularListLeft = reverse . transpose

rotateRectangularListRight :: [[a]] -> [[a]]
rotateRectangularListRight = transpose . reverse

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
solveDay8Part2 = maximum . map maximum . scenicScores

scenicScores :: Day8Input -> [[Int]]
scenicScores x = mulElements (firstSide x) $ mulElements (secondSide x) $ mulElements (thirdSide x) (fourthSide x)
    where mulElements = zipWith (zipWith (*))
          firstSide (Day8Input hs) = scenicScoreFromLeft hs
          secondSide (Day8Input hs) = rotateRectangularListLeft $ scenicScoreFromLeft $ rotateRectangularListRight hs
          thirdSide (Day8Input hs) = rotateRectangularListLeft $ rotateRectangularListLeft $ scenicScoreFromLeft $ rotateRectangularListRight $ rotateRectangularListRight hs
          fourthSide (Day8Input hs) = rotateRectangularListRight $ scenicScoreFromLeft $ rotateRectangularListLeft hs


-- Well, that sucks. Part 1 result is unusable for part 2... But we can do a scanl to transform each tree in each of the 4 sides to its viewing distance.
-- Or, probably it's easier to just write the function instead of using scanl.
-- And once that's döner, we can rotate everything back to the original orientation, multiply up and boom, partay!

newtype DistanceSinceHeight = DistanceSinceHeight (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

getDistanceSinceHeight :: DistanceSinceHeight -> Height -> Int
getDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Zero = ze
getDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) One = on
getDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Two = tw
getDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Three = th
getDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Four = fo
getDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Five = fi
getDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Six = si
getDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Seven = se
getDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Eight = ei
getDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Nine = ni

stepDistanceSinceHeight :: DistanceSinceHeight -> Height -> DistanceSinceHeight
stepDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Zero = DistanceSinceHeight (1,on+1,tw+1,th+1,fo+1,fi+1,si+1,se+1,ei+1,ni+1)
stepDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) One = DistanceSinceHeight (1,1,tw+1,th+1,fo+1,fi+1,si+1,se+1,ei+1,ni+1)
stepDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Two = DistanceSinceHeight (1,1,1,th+1,fo+1,fi+1,si+1,se+1,ei+1,ni+1)
stepDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Three = DistanceSinceHeight (1,1,1,1,fo+1,fi+1,si+1,se+1,ei+1,ni+1)
stepDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Four = DistanceSinceHeight (1,1,1,1,1,fi+1,si+1,se+1,ei+1,ni+1)
stepDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Five = DistanceSinceHeight (1,1,1,1,1,1,si+1,se+1,ei+1,ni+1)
stepDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Six = DistanceSinceHeight (1,1,1,1,1,1,1,se+1,ei+1,ni+1)
stepDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Seven = DistanceSinceHeight (1,1,1,1,1,1,1,1,ei+1,ni+1)
stepDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Eight = DistanceSinceHeight (1,1,1,1,1,1,1,1,1,ni+1)
stepDistanceSinceHeight (DistanceSinceHeight (ze,on,tw,th,fo,fi,si,se,ei,ni)) Nine = DistanceSinceHeight (1,1,1,1,1,1,1,1,1,1)

scenicScoreFromLeft :: [[Height]] -> [[Int]]
scenicScoreFromLeft = map scenicScoreFromLeftForRow
scenicScoreFromLeftForRow :: [Height] -> [Int]
scenicScoreFromLeftForRow = map fst . tail . scanl scenicScanner (-1, DistanceSinceHeight (0,0,0,0,0,0,0,0,0,0))
    where scenicScanner (_, d) h = (getDistanceSinceHeight d h, stepDistanceSinceHeight d h)