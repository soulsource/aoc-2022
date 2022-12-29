module Main (main) where

import System.Environment ( getArgs )
import Text.Read (readMaybe)
import Data.Bifunctor (bimap, first)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Control.Monad.Zip (MonadZip(mzip))

main :: IO ()
main = getArgs >>= readFile . head >>= print . solveDay9

solveDay9 :: String -> String
solveDay9 = formatDay9Result . fmap solveDay9Parts . parseDay9Input

formatDay9Result :: Maybe (Int, Int) -> String
formatDay9Result Nothing = "Failed to read input for day9."
formatDay9Result (Just (p1, p2)) = "Part 1: " ++ show p1 ++ ", Part 2: " ++ show p2

-- Imho it's stupid that Haskell by default imports Right and Left.
-- Also, that there's no dedicated Result type, and rather Either is used.
data Direction = MoveUp | MoveRight | MoveDown | MoveLeft -- part 1 only uses these directions.
               | MoveUpRight | MoveUpLeft | MoveDownRight | MoveDownLeft -- these four directions are needed for part 2.

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

tailPosition :: RopePosition -> AbsoluteTail
tailPosition (RopePosition (AbsoluteHead (x,y)) (RelativeTail (dx, dy))) = AbsoluteTail (applyOffset x dx, applyOffset y dy)
    where applyOffset i MinusOne = i-1
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
-- The variants below are used for part 2.
virtualizeRelativeTail (RelativeTail (x,y)) (Command MoveUpRight) = VirtualRelativeTail (decrementOffset x, decrementOffset y)
virtualizeRelativeTail (RelativeTail (x,y)) (Command MoveUpLeft) = VirtualRelativeTail (incrementOffset x, decrementOffset y)
virtualizeRelativeTail (RelativeTail (x,y)) (Command MoveDownRight) = VirtualRelativeTail (decrementOffset x, incrementOffset y)
virtualizeRelativeTail (RelativeTail (x,y)) (Command MoveDownLeft) = VirtualRelativeTail (incrementOffset x, incrementOffset y)

-- Makes the tail realize it's still connected to the head. Second part of output is used in Part2 of the riddle. Part1 only needs the RelativeTail.
realizeVirtualTail :: VirtualRelativeTail -> (RelativeTail, Maybe Command)
realizeVirtualTail v = maybe (realizeVirtualTailByMoving v) (\x -> (x, Nothing)) (tryRealizeVirtualTailWithoutMoving v)

tryRealizeVirtualTailWithoutMoving :: VirtualRelativeTail -> Maybe RelativeTail
tryRealizeVirtualTailWithoutMoving (VirtualRelativeTail (x,y)) = RelativeTail <$> mzip (tryRealizeOffsetWithoutMoving x) (tryRealizeOffsetWithoutMoving y)

tryRealizeOffsetWithoutMoving :: VirtualRelativeOffset -> Maybe AllowedRelativeOffset
tryRealizeOffsetWithoutMoving VMinusOne = Just MinusOne
tryRealizeOffsetWithoutMoving VZero = Just Zero
tryRealizeOffsetWithoutMoving VPlusOne = Just PlusOne
tryRealizeOffsetWithoutMoving _ = Nothing

-- as with realizeVirtualTail: The second field in the return value is only used by Part2 of the puzzle.
realizeVirtualTailByMoving :: VirtualRelativeTail -> (RelativeTail, Maybe Command)
realizeVirtualTailByMoving (VirtualRelativeTail (x,y)) = (RelativeTail (moveOffsetTowardsZero x, moveOffsetTowardsZero y), toCommand (x,y))
    where toCommand (x,y) | isNegative x && isNegative y = Just $ Command MoveUpRight -- both negative
          toCommand (x,VZero) | isNegative x = Just $ Command MoveRight               -- x negative, y zero
          toCommand (x,y) | isNegative x = Just $ Command MoveDownRight               -- x negative, y positive
          toCommand (VZero,y) | isNegative y = Just $ Command MoveUp                  -- x zero, y negative
          toCommand (VZero,VZero) = Nothing                                           -- x zero, y zero (should be unreachable, but well, this is a top level function, so better have it here)
          toCommand (VZero,y) = Just $ Command MoveDown                               -- x zero, y positive
          toCommand (x,y) | isNegative y = Just $ Command MoveUpLeft                  -- x positive, y negative
          toCommand (x,VZero) = Just $ Command MoveLeft                               -- x positive, y zero
          toCommand (x,y) = Just $ Command MoveDownLeft                               -- x positive, y positive
          isNegative VMinusOne = True
          isNegative VMinusTwo = True
          isNegative _ = False

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
-- The variants below are used for part 2.
applyCommandToAbsoluteHead (AbsoluteHead (x,y)) (Command MoveUpRight) = AbsoluteHead (x+1,y+1)
applyCommandToAbsoluteHead (AbsoluteHead (x,y)) (Command MoveUpLeft) = AbsoluteHead (x-1,y+1)
applyCommandToAbsoluteHead (AbsoluteHead (x,y)) (Command MoveDownRight) = AbsoluteHead (x+1,y-1)
applyCommandToAbsoluteHead (AbsoluteHead (x,y)) (Command MoveDownLeft) = AbsoluteHead (x-1,y-1)

applyCommand :: RopePosition -> Command -> RopePosition
applyCommand (RopePosition head tail) c = RopePosition (applyCommandToAbsoluteHead head c) (fst $ realizeVirtualTail $ virtualizeRelativeTail tail c)


-- This can actually be resolved in a very stupid - erm - I mean smart way. A rope of length 9 is just the same as 9 ropes of length 1 bound together.
-- Sooo, what we have to do is to chain 9 ropes by converting the movement of each tail back to a command for the next head.
-- This means we need to extend the Command type with diagonal movement though.

-- Note to self: To keep this safe, extend the return type of realizeVirtualTail. Let it return a tuple (RelativeTail, Maybe Command).
--               That way, the data needed for the next rope segment can be extracted at the point where it doesn't
--               require the introduction of partial functions.
--               This also means extending the applyCommand function signature, so that instead of RopePosition it works on (RopePosition, Maybe Command).
--               Or, a custom applyCommandPart2 function that does that.

-- the final flow will be a chain of [Command] -> [Command] mappings, only the last one will be - well, the last one will be solvePart1 :D

solveDay9Part2 :: [Command] -> Int
solveDay9Part2 = solveDay9Part1 . last . take 9 . iterate moveRopePart -- 9, because iterate also returns the unmodified input as first element.

moveRopePart :: [Command] -> [Command]
moveRopePart = mapMaybe snd . scanl applyCommandPart2 (startingRopePosition, Nothing)

applyCommandPart2 :: (RopePosition, Maybe Command) -> Command -> (RopePosition, Maybe Command)
applyCommandPart2 (RopePosition head tail, _) c = first (RopePosition (applyCommandToAbsoluteHead head c)) $ realizeVirtualTail $ virtualizeRelativeTail tail c