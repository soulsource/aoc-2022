module Main (main) where

import System.Environment ( getArgs )
import Text.Read (readMaybe)
import Data.Bifunctor (bimap)

main :: IO ()
main = getArgs >>= readFile . head >>= putStr . solveDay10

solveDay10 :: String -> String
solveDay10 = formatDay10Result . fmap solveDay10Parts . parseDay10Input

formatDay10Result :: Either String (Int, String) -> String
formatDay10Result (Left s) = s
formatDay10Result (Right (p1, p2)) = "Part 1: " ++ show p1 ++ "\n\nPart 2:\n" ++ p2

data Command = NoOp | AddX Int

parseDay10Input :: String -> Either String [Command]
parseDay10Input = mapM parseCommand . lines

parseCommand :: String -> Either String Command
parseCommand "noop" = Right NoOp
parseCommand ('a':'d':'d':'x':' ':ss) =  maybe (Left "Invalid parameter for addx instruction") (Right . AddX) $ readMaybe ss 
parseCommand x = Left $ "Unknown instruction: " ++ x

solveDay10Parts :: [Command] -> (Int, String)
solveDay10Parts = bimap solveDay9Part1 solveDay9Part2 . dup
    where dup x = (x,x)

newtype CycleCount = CycleCount Int deriving (Eq, Ord)
newtype RegisterValue = RegisterValue Int
newtype SignalStrength = SignalStrength Int
newtype CpuState = CpuState (CycleCount, RegisterValue)

newtype DeltaCpuStates = DeltaCpuStates [CpuState]
unDeltaCpuStates :: DeltaCpuStates -> [CpuState]
unDeltaCpuStates (DeltaCpuStates d) = d

initialCpuState :: CpuState
initialCpuState = CpuState (CycleCount 1, RegisterValue 1)

getCycleCount :: CpuState -> CycleCount
getCycleCount (CpuState (c,_)) = c

nextCycle :: CycleCount -> CycleCount
nextCycle (CycleCount c) = CycleCount (c+1)

unSignalStrength :: SignalStrength -> Int
unSignalStrength (SignalStrength a) = a

signalStrength :: CycleCount -> RegisterValue -> SignalStrength
signalStrength (CycleCount c) (RegisterValue r) = SignalStrength (c*r)

getRegisterValue :: CpuState -> RegisterValue
getRegisterValue (CpuState (_,r)) = r

solveDay9Part1 :: [Command] -> Int 
solveDay9Part1 = getSumOfRelevantSignalStrengths . cpuStates

cpuStates :: [Command] -> DeltaCpuStates -- this could be solved using scanl, but the problem is easier to solve using a dedicated worker function.
cpuStates = cpuStatesWorker initialCpuState

cpuStatesWorker :: CpuState -> [Command] -> DeltaCpuStates
cpuStatesWorker c [] = DeltaCpuStates $ return c
cpuStatesWorker (CpuState (CycleCount c, r)) (NoOp:cs) = cpuStatesWorker (CpuState (CycleCount (c+1), r)) cs
cpuStatesWorker (CpuState (CycleCount c, RegisterValue r)) ((AddX x):cs) = DeltaCpuStates $ newState:unDeltaCpuStates (cpuStatesWorker newState cs)
    where newState = CpuState (CycleCount (c+2), RegisterValue (r+x))

getSumOfRelevantSignalStrengths :: DeltaCpuStates -> Int
getSumOfRelevantSignalStrengths s = sum $ map (unSignalStrength . getSignalStrengthAtCycleCount s . CycleCount) $ take 6 $ iterate (+40) 20

getSignalStrengthAtCycleCount :: DeltaCpuStates -> CycleCount -> SignalStrength
getSignalStrengthAtCycleCount s c = signalStrength c $ getRegisterAtCycleCount s c

getRegisterAtCycleCount :: DeltaCpuStates -> CycleCount -> RegisterValue
getRegisterAtCycleCount s c = getRegisterValue $ last $ takeWhile (\(CpuState (cc,_)) -> cc <= c) $ unDeltaCpuStates s

-- Part 2 is basically: for each cycle, check if RegisterValue is CycleCount-1, CycleCount, or CycleCount+1, and if yes, print '#', otherwise print '.'.

newtype ScreenWidth = ScreenWidth Int

solveDay9Part2 :: [Command] -> String
solveDay9Part2 = addNewlines (ScreenWidth 40) . take 240 . map (toPixel (ScreenWidth 40)) . expandCycles . cpuStates

expandCycles :: DeltaCpuStates -> [CpuState]
expandCycles (DeltaCpuStates c) = expandCyclesWorker c $ CycleCount 1
    where expandCyclesWorker (c1:cs) i | getCycleCount c1 > i = CpuState (i,RegisterValue 1):expandCyclesWorker (c1:cs) (nextCycle i) -- first few steps
          -- If we have still at least two states, we have to check if the current is still good, if not we have to advance.
          expandCyclesWorker (c1:c2:cs) i | getCycleCount c2 > i = CpuState (i,getRegisterValue c1):expandCyclesWorker (c1:c2:cs) (nextCycle i)
          expandCyclesWorker (c1:c2:cs) i = expandCyclesWorker (c2:cs) i
          -- If we are at the end, repeat last value infinitely.
          expandCyclesWorker (c:_) i = CpuState (i, getRegisterValue c):expandCyclesWorker [c] (nextCycle i) 


toPixel :: ScreenWidth -> CpuState -> Char
toPixel (ScreenWidth w) (CpuState (CycleCount c, RegisterValue r)) = if isBrightPixel then '#' else '.'
    where isBrightPixel = abs (((c-1) `mod` w)-r) <=1 -- c-1 because pixels start at 0, cycles start at 1... Because of course they do.

addNewlines :: ScreenWidth -> String -> String
addNewlines (ScreenWidth i) = addNewLinesWorker i 0
    where addNewLinesWorker every current (c:cs) | current /= 0 && current `mod` every == 0 = '\n':c:addNewLinesWorker every (current + 1) cs
          addNewLinesWorker every current (c:cs) = c:addNewLinesWorker every (current + 1) cs
          addNewLinesWorker every current [] = "\n"


