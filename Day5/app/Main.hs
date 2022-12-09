module Main (main) where

import System.Environment ( getArgs )
import Data.Bifunctor (first, second)

main :: IO ()
main = getArgs >>= readFile . head >>= print . solveDay5

solveDay5 :: String -> String
solveDay5 input = solveDay5Part1 x ++ ", " ++ solveDay5Part2 x
    where x = parseDay5Input input

data Day5Input = Day5Input {
    state :: Stacks,
    commands :: Commands
}

newtype Stack = Stack [Char]
newtype Stacks = Stacks [Stack] -- Encoding: First element is topmost crate.

-- Destructure Stacks
unStacks :: Stacks -> [Stack]
unStacks (Stacks x) = x

newtype Commands = Commands [Command]

data Command = Move {
    amount :: Int,
    source :: Int,
    destination :: Int
}

parseDay5Input :: String -> Day5Input
parseDay5Input input = Day5Input { state = parseStacks x, commands = parseCommands y}
    where (x,y) = separateStacksFromCommands $ lines input

separateStacksFromCommands :: [String] -> ([String],[String])
separateStacksFromCommands ([]:bs) = ([],bs)
separateStacksFromCommands (a:as) = Data.Bifunctor.first (a:) $ separateStacksFromCommands as

-- let's fill the data from bottom to top.
parseStacks :: [String] -> Stacks
parseStacks = foldl addStackLine (Stacks []) . drop 1 . reverse

addStackLine :: Stacks -> String -> Stacks
addStackLine s (' ':'[':a:']':as) = addStackLine s ('[':a:']':as)
addStackLine (Stacks (s:ss)) (' ':' ':' ':' ':as) = Stacks (s : unStacks (addStackLine (Stacks ss) as))
addStackLine (Stacks (s:ss)) (' ':' ':' ':as) = Stacks (s : unStacks (addStackLine (Stacks ss) as))
addStackLine (Stacks (s:ss)) ('[':a:']':as) = Stacks (addToStack s a : unStacks (addStackLine (Stacks ss) as))
    where addToStack (Stack s) a = Stack (a:s)
addStackLine (Stacks []) ('[':a:']':as) = Stacks (Stack [a] : unStacks (addStackLine (Stacks []) as))
addStackLine s [] = s
addStackLine _ s = error ("Malformed input" ++ s)
-- With malformed input, there would be patterns we won't catch. But our input is well-formed.

parseCommands :: [String] -> Commands
parseCommands = Commands . map parseCommand

parseCommand :: String -> Command
parseCommand = parseWordedCommand . words
    where parseWordedCommand ("move":details) = parseMove details
              where parseMove [count, "from", origin, "to", target] = Move {amount = read count, source = read origin, destination = read target}
          parseWordedCommand _ = error "Unsupported Command."

solveDay5Part1 :: Day5Input -> String
solveDay5Part1 Day5Input {state = state, commands = Commands c} = printTops $ foldl (flip applyCommand) state c

applyCommand :: Command -> Stacks -> Stacks
applyCommand (Move 1 source destination) = moveSingle source destination
applyCommand (Move amount source destination) = applyCommand (Move (amount-1) source destination) . moveSingle source destination

moveSingle :: Int -> Int -> Stacks -> Stacks
moveSingle from to = placeCrate to . takeCrate from

placeCrate :: Int -> (Char, Stacks) -> Stacks
placeCrate 1 (crate, Stacks ((Stack s):ss)) = Stacks (Stack (crate:s):ss)
placeCrate n (crate, Stacks (s:ss)) = Stacks $ s : unStacks (placeCrate (n-1) (crate, Stacks ss))

takeCrate :: Int -> Stacks -> (Char, Stacks)
takeCrate 1 (Stacks ((Stack (s:ss)):sss)) = (s, Stacks (Stack ss:sss))
takeCrate n (Stacks (Stack s:ss)) | n > 1 = second (\x -> Stacks (Stack s:unStacks x)) (takeCrate (n-1) (Stacks ss))
-- Should maybe add this to all take/place?
-- takeCrate _ (Stacks []) = error "Index for Take is out of bounds."
-- takeCrate 1 (Stacks ((Stack []):sss)) = error "Attempted to take a crate from an empty stockpile."
-- takeCrate n _ | n < 1 = error "Index for Take is out of bounds."

printTops :: Stacks -> String
printTops (Stacks s) = foldr ((:) . getTop) "" s

getTop :: Stack -> Char
getTop (Stack s) = head s

solveDay5Part2 :: Day5Input -> String
solveDay5Part2 Day5Input {state = state, commands = Commands c} = printTops $ foldl (flip applyCommandNewerCrane) state c

applyCommandNewerCrane :: Command -> Stacks -> Stacks
applyCommandNewerCrane (Move amount source destination) = moveMany amount source destination

moveMany :: Int -> Int -> Int -> Stacks -> Stacks
moveMany count from to = placeMany to . takeMany count from

placeMany :: Int -> (Stack, Stacks) -> Stacks
placeMany 1 (Stack crates, Stacks ((Stack s):ss)) = Stacks (Stack (crates ++ s):ss)
placeMany from (crates, Stacks (s:ss)) = Stacks $ s : unStacks (placeMany (from-1) (crates, Stacks ss))

takeMany :: Int -> Int -> Stacks -> (Stack, Stacks)
takeMany count 1 (Stacks ((Stack s):ss)) = (Stack taken,Stacks (Stack rest:ss))
    where taken = take count s
          rest = drop count s
takeMany count n (Stacks (Stack s:ss)) = second (\x -> Stacks (Stack s:unStacks x)) (takeMany count (n-1) (Stacks ss))