module Main (main) where

import System.Environment ( getArgs )
import Text.Read (readMaybe)
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Control.Monad.Zip (MonadZip(mzip))
import Control.Exception (assert)

main :: IO ()
main = getArgs >>= readFile . head >>= print . solveDay9

solveDay9 :: String -> String
solveDay9 = formatDay9Result . fmap solveDay9Parts . parseDay9Input

formatDay9Result :: Maybe (Int, Int) -> String
formatDay9Result Nothing = "Failed to read input for day9."
formatDay9Result (Just (p1, p2)) = "Part 1: " ++ show p1 ++ ", Part 2: " ++ show p2

-- Imho it's stupid that Haskell by default imports Right and Left.
-- Also, that there's no dedicated Result type, and rather Either is used.
data Direction = MoveUp | MoveRight | MoveDown | MoveLeft

data MultiCommand = MultiCommand Direction Word -- using Word here gives a "is >= 0" check in readMaybe for free (I think).
newtype Command = Command Direction

parseDay9Input :: String -> Maybe [MultiCommand]
parseDay9Input = mapM readInputLine . lines

readInputLine :: String -> Maybe MultiCommand
readInputLine ('U':' ':n) = MultiCommand MoveUp <$> readMaybe n
readInputLine ('R':' ':n) = MultiCommand MoveRight <$> readMaybe n
readInputLine ('D':' ':n) = MultiCommand MoveDown <$> readMaybe n
readInputLine ('L':' ':n) = MultiCommand MoveLeft <$> readMaybe n
readInputLine _ = Nothing

solveDay9Parts :: [MultiCommand] -> (Int, Int)
solveDay9Parts =  bimap solveDay9Part1 solveDay9Part2 . dup . unMultiCommands
    where dup a = (a,a)

unMultiCommands :: [MultiCommand] -> [Command]
unMultiCommands = concatMap unMultiCommand

unMultiCommand :: MultiCommand -> [Command]
unMultiCommand (MultiCommand _ 0) = []
unMultiCommand (MultiCommand d n) = Command d:unMultiCommand (MultiCommand d (n-1))

data AllowedRelativeOffset = MinusOne | Zero | PlusOne
newtype RelativeTail = RelativeTail (AllowedRelativeOffset, AllowedRelativeOffset)
newtype AbsoluteHead = AbsoluteHead (Int, Int)
newtype AbsoluteTail = AbsoluteTail (Int, Int) deriving (Eq, Ord)

data RopePosition = RopePosition AbsoluteHead RelativeTail
startingRopePosition :: RopePosition
startingRopePosition = RopePosition (AbsoluteHead (0,0)) (RelativeTail (Zero, Zero))

solveDay9Part1 :: [Command] -> Int
solveDay9Part1 = length . Set.fromList . tailPositions . ropePositions -- building a set is cheaper than making sure every element in the list is unique.

tailPositions :: [RopePosition] -> [AbsoluteTail]
tailPositions = map tailPosition
    where tailPosition (RopePosition (AbsoluteHead (x,y)) (RelativeTail (dx, dy))) = AbsoluteTail (applyOffset x dx, applyOffset y dy)
          applyOffset i MinusOne = i-1
          applyOffset i Zero = i
          applyOffset i PlusOne = i+1

ropePositions :: [Command] -> [RopePosition]
ropePositions = scanl applyCommand startingRopePosition -- this could also be `tail . scanl applyCommand startingRopePosition` but that would ignore starting pos.

data VirtualRelativeOffset = VMinusTwo | VMinusOne | VZero | VPlusOne | VPlusTwo
newtype VirtualRelativeTail = VirtualRelativeTail (VirtualRelativeOffset, VirtualRelativeOffset)

-- Where, after applying the command to the head, the tail would be if it weren't connected.
virtualizeRelativeTail :: RelativeTail -> Command -> VirtualRelativeTail
virtualizeRelativeTail (RelativeTail (x,y)) (Command MoveUp) = VirtualRelativeTail (virtualFromRelativeOffset x, decrementOffset y)
virtualizeRelativeTail (RelativeTail (x,y)) (Command MoveDown) = VirtualRelativeTail (virtualFromRelativeOffset x, incrementOffset y)
virtualizeRelativeTail (RelativeTail (x,y)) (Command MoveRight) = VirtualRelativeTail (decrementOffset x, virtualFromRelativeOffset y)
virtualizeRelativeTail (RelativeTail (x,y)) (Command MoveLeft) = VirtualRelativeTail (incrementOffset x, virtualFromRelativeOffset y)

-- Makes the tail realize it's still connected to the head.
realizeVirtualTail :: VirtualRelativeTail -> RelativeTail
realizeVirtualTail v = fromMaybe (realizeVirtualTailByMoving v) (tryRealizeVirtualTailWithoutMoving v)

tryRealizeVirtualTailWithoutMoving :: VirtualRelativeTail -> Maybe RelativeTail
tryRealizeVirtualTailWithoutMoving (VirtualRelativeTail (x,y)) = RelativeTail <$> mzip (tryRealizeOffsetWithoutMoving x) (tryRealizeOffsetWithoutMoving y)

tryRealizeOffsetWithoutMoving :: VirtualRelativeOffset -> Maybe AllowedRelativeOffset
tryRealizeOffsetWithoutMoving VMinusOne = Just MinusOne
tryRealizeOffsetWithoutMoving VZero = Just Zero
tryRealizeOffsetWithoutMoving VPlusOne = Just PlusOne
tryRealizeOffsetWithoutMoving _ = Nothing

realizeVirtualTailByMoving :: VirtualRelativeTail -> RelativeTail
realizeVirtualTailByMoving (VirtualRelativeTail (x,y)) = RelativeTail (moveOffsetTowardsZero x, moveOffsetTowardsZero y)

moveOffsetTowardsZero :: VirtualRelativeOffset -> AllowedRelativeOffset
moveOffsetTowardsZero VMinusTwo = MinusOne
moveOffsetTowardsZero VMinusOne = Zero
moveOffsetTowardsZero VZero = Zero
moveOffsetTowardsZero VPlusOne = Zero
moveOffsetTowardsZero vPlusTwo = PlusOne

virtualFromRelativeOffset :: AllowedRelativeOffset -> VirtualRelativeOffset
virtualFromRelativeOffset MinusOne = VMinusOne
virtualFromRelativeOffset Zero = VZero
virtualFromRelativeOffset PlusOne = VPlusOne

decrementOffset :: AllowedRelativeOffset -> VirtualRelativeOffset
decrementOffset MinusOne = VMinusTwo
decrementOffset Zero = VMinusOne
decrementOffset PlusOne = VZero

incrementOffset :: AllowedRelativeOffset -> VirtualRelativeOffset
incrementOffset MinusOne = VZero
incrementOffset Zero = VPlusOne
incrementOffset PlusOne = VPlusTwo

applyCommandToAbsoluteHead :: AbsoluteHead -> Command -> AbsoluteHead
applyCommandToAbsoluteHead (AbsoluteHead (x,y)) (Command MoveUp) = AbsoluteHead (x,y+1)
applyCommandToAbsoluteHead (AbsoluteHead (x,y)) (Command MoveDown) = AbsoluteHead (x,y-1)
applyCommandToAbsoluteHead (AbsoluteHead (x,y)) (Command MoveRight) = AbsoluteHead (x+1,y)
applyCommandToAbsoluteHead (AbsoluteHead (x,y)) (Command MoveLeft) = AbsoluteHead (x-1,y)

applyCommand :: RopePosition -> Command -> RopePosition
applyCommand (RopePosition head tail) c = RopePosition (applyCommandToAbsoluteHead head c) (realizeVirtualTail $ virtualizeRelativeTail tail c)

solveDay9Part2 :: [Command] -> Int
solveDay9Part2 = undefined