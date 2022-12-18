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
parseCommands = map parseCommand . skipEmptyLines . splitAtDollarSign
    where skipEmptyLines = filter (/= "") -- splitting at all dollar signs can yield empty commands, for instance if the file starts with $.

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
buildDirectoryFromCommands = snd . foldl applyCommandToDirectory (Path [], Directory "/" [] [])

applyCommandToDirectory :: (Path, Directory) -> Command -> (Path, Directory)
applyCommandToDirectory (oldPath, dir) CdRoot = (Path [], dir)
applyCommandToDirectory (oldPath, dir) CdUp = (fst . popDirectory $ oldPath, dir)
applyCommandToDirectory (oldPath, dir) (Cd subDir) = (pushDirectory oldPath subDir, dir)
applyCommandToDirectory (path, oldDir) (Ls contents) = (path, addContentsToDirectory path oldDir $ parseLsOutput contents)

addContentsToDirectory :: Path -> Directory -> ([Directory], [File]) -> Directory
addContentsToDirectory (Path []) (Directory name dirs files) (newDirs, newFiles) = Directory name (newDirs ++ dirs) (newFiles ++ files)
addContentsToDirectory (Path p) (Directory name dirs files) n = Directory name (addContentsToDirectory (Path $ init p) subDir n:otherSubDirs) files
    where (subDir, otherSubDirs) = getSubDir $ break (\(Directory name _ _) -> name == lp) dirs
          getSubDir (a, b:bs) = (b, a ++ bs)
          getSubDir (a, []) = error "Cannot descend into an unknown subdirectory."
          lp = last p


data DirOrFile = Dir Directory | Fil File
parseLsOutput :: String -> ([Directory], [File])
parseLsOutput = foldl appendLsLine ([],[]) . map parseLsLine . lines
    where appendLsLine (dirs, files) (Dir d) = (d:dirs, files)
          appendLsLine (dirs, files) (Fil f) = (dirs, f:files)

parseLsLine :: String -> DirOrFile
parseLsLine ('d':'i':'r':' ':dn) = Dir $ Directory dn [] []
parseLsLine fileSizeAndName = Fil $ File nn (read sz)
    where (sz,nn) = verifyWordCount $ words fileSizeAndName
          verifyWordCount [a,b] = (a,b) -- could use literal [sz,nn] pattern instead, but that doesn't give a nice error message. This does.
          verifyWordCount _ = error "Wrong number of words in ls file entry."

solveDay7Parts :: Directory -> (Int, Int)
solveDay7Parts d = (solveDay7Part1 d, solveDay7Part2 d)

data DirWithAccumulatedSize = DirWithAccumulatedSize String Int [DirWithAccumulatedSize]
newtype DirWithAcuumulatedSizeFlat = DirWithAcuumulatedSizeFlat (String, Int)

solveDay7Part1 :: Directory -> Int
solveDay7Part1 = 
    sum
     . map (\(DirWithAcuumulatedSizeFlat (_,s)) -> s)
     . filter (\(DirWithAcuumulatedSizeFlat (n,s)) -> s <= 100000)
     . flattenDirWithAccumulatedSize 
     . convertToAccumulatedSizeTree

convertToAccumulatedSizeTree :: Directory -> DirWithAccumulatedSize
convertToAccumulatedSizeTree (Directory name subdirs files) = 
    DirWithAccumulatedSize name (sum subdirSizes + sum fileSizes) sizedSubdirs
    where sizedSubdirs = map convertToAccumulatedSizeTree subdirs
          fileSizes = map (\(File _ size) -> size) files
          subdirSizes = map getSize sizedSubdirs

flattenDirWithAccumulatedSize :: DirWithAccumulatedSize -> [DirWithAcuumulatedSizeFlat]
flattenDirWithAccumulatedSize (DirWithAccumulatedSize name size []) = [DirWithAcuumulatedSizeFlat (name, size)]
flattenDirWithAccumulatedSize (DirWithAccumulatedSize name size (sd:sds)) = flattenDirWithAccumulatedSize sd ++ flattenDirWithAccumulatedSize (DirWithAccumulatedSize name size sds)

solveDay7Part2 :: Directory -> Int
solveDay7Part2 d = getSize $ smallestDirLargerThan requiredSpace flattenedDirs
    where flattenedDirs = flattenDirWithAccumulatedSize accumulatedSizeTree
          accumulatedSizeTree = convertToAccumulatedSizeTree d
          requiredSpace = getSize accumulatedSizeTree - (70000000 - 30000000)

smallestDirLargerThan :: Int -> [DirWithAcuumulatedSizeFlat] -> DirWithAcuumulatedSizeFlat
smallestDirLargerThan m = foldl1 smallestSize . filter ((m <=) . getSize) -- too lazy to handle empty list case -> foldl1
    where smallestSize d1 d2 = if getSize d1 < getSize d2 then d1 else d2

class DirSize a where
    getSize :: a -> Int

instance DirSize DirWithAcuumulatedSizeFlat where
    getSize (DirWithAcuumulatedSizeFlat (_,s)) = s

instance DirSize DirWithAccumulatedSize where
    getSize (DirWithAccumulatedSize _ s _) = s