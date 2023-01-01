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

unSignalStrength :: SignalStrength -> Int
unSignalStrength (SignalStrength a) = a

signalStrength :: CycleCount -> RegisterValue -> SignalStrength
signalStrength (CycleCount c) (RegisterValue r) = SignalStrength (c*r)

getRegisterValue :: CpuState -> RegisterValue
getRegisterValue (CpuState (_,r)) = r

solveDay9Part1 :: [Command] -> Int 
solveDay9Part1 = getSumOfRelevantSignalStrengths . cpuStates

cpuStates :: [Command] -> [CpuState] -- this could be solved using scanl, but the problem is easier to solve using a dedicated worker function.
cpuStates = cpuStatesWorker (CpuState (CycleCount 1, RegisterValue 1)) 

cpuStatesWorker :: CpuState -> [Command] -> [CpuState]
cpuStatesWorker c [] = return c
cpuStatesWorker (CpuState (CycleCount c, r)) (NoOp:cs) = cpuStatesWorker (CpuState (CycleCount (c+1), r)) cs
cpuStatesWorker (CpuState (CycleCount c, RegisterValue r)) ((AddX x):cs) = newState:cpuStatesWorker newState cs
    where newState = CpuState (CycleCount (c+2), RegisterValue (r+x))

getSumOfRelevantSignalStrengths :: [CpuState] -> Int
getSumOfRelevantSignalStrengths s = sum $ map (unSignalStrength . getSignalStrengthAtCycleCount s . CycleCount) $ take 6 $ iterate (+40) 20

getSignalStrengthAtCycleCount :: [CpuState] -> CycleCount -> SignalStrength
getSignalStrengthAtCycleCount s c = signalStrength c $ getRegisterAtCycleCount s c

getRegisterAtCycleCount :: [CpuState] -> CycleCount -> RegisterValue
getRegisterAtCycleCount s c = getRegisterValue $ last $ takeWhile (\(CpuState (cc,_)) -> cc <= c) s

solveDay9Part2 :: [Command] -> String
solveDay9Part2 c = "Not yet done, sorry.\n"