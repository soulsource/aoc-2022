module Main (main) where

import System.Environment ( getArgs )

main :: IO ()
main = getArgs >>= readFile . head >>= print . solveDay7

solveDay7 :: String -> String
solveDay7 = formatSolutions . solveDay7Parts . parseDay7Input
    where formatSolutions (x,y) = show x ++ ", " ++ show y

data File = File String Int
data Directory = Directory String [Directory] [File]

data Command = CdRoot | CdUp | Cd String | Ls String

newtype Path = Path [String] -- Just a list of directory names. Root is LAST element.

pushDirectory :: Path -> String -> Path
pushDirectory (Path p) = Path . (:p)

popDirectory :: Path -> (Path, String)
popDirectory (Path (p:ps)) = (Path ps, p)
popDirectory _ = error "Cannot pop a directory from an empty path."

parseDay7Input :: String -> Directory
parseDay7Input = buildDirectoryFromCommands . parseCommands

parseCommands :: String -> [Command]
parseCommands = map parseCommand . splitAtDollarSign

splitAtDollarSign :: String -> [String]
splitAtDollarSign [] = []
splitAtDollarSign ('$':' ':bs) = []:splitAtDollarSign bs
splitAtDollarSign (b:bs) = prependToFirst (splitAtDollarSign bs) b
    where prependToFirst (l:ls) b = (b:l):ls
          prependToFirst [] b = [[b]]

parseCommand :: String -> Command
parseCommand ('c':'d':' ':'/':_) = CdRoot
parseCommand ('c':'d':' ':'.':'.':_) = CdUp
parseCommand ('c':'d':' ':path) = Cd $ stripTrailingNewline path
parseCommand ('l':'s':'\n':files) = Ls $ stripTrailingNewline files
parseCommand e = error ("Invalid command: " ++ e)

stripTrailingNewline :: String -> String
stripTrailingNewline ['\n'] = []
stripTrailingNewline [] = []
stripTrailingNewline (a:as) = a:stripTrailingNewline as

buildDirectoryFromCommands :: [Command] -> Directory
buildDirectoryFromCommands = error "unimplemented"

applyCommandToDirectory :: (Path, Directory) -> Command -> (Path, Directory)
applyCommandToDirectory (oldPath, oldDir) CdRoot = (Path [], oldDir)
applyCommandToDirectory (oldPath, oldDir) CdUp = (fst . popDirectory $ oldPath, oldDir)
-- todo: other arms

solveDay7Parts :: Directory -> (Int, Int)
solveDay7Parts = error "unimplemented"