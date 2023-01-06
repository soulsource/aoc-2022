module Main where

import MonkeyBrain (evaluateIntExpression, tryParseExpression, Expression, ExpressionParseError(..), VariableNameValidator (..), VariableValues (..), Variable (..))
import Data.Maybe (fromJust)
import System.Environment ( getArgs )
import Data.Bifunctor (bimap)
import Text.Read (readMaybe)
import Control.Monad ((<=<))

main :: IO ()
main = getArgs >>= readFile . head >>= putStr . solveDay11

solveDay11 :: String -> String
solveDay11 = formatDay11Result . fmap solveDay11Parts . parseDay11Input

formatDay11Result :: (Show a1, Show a2, Show e) => Either e (a1, a2) -> String
formatDay11Result (Right (p1, p2)) = "Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2 ++ "\n"
formatDay11Result (Left e) = "Error: " ++ show e ++ "\n"

newtype WorryLevel = WorryLevel { unWorryLevel :: Int}
newtype ItemId = ItemId (MonkeyId,Int)
newtype MonkeyId = MonkeyId Int deriving (Eq, Show)
newtype StartingItems = StartingItems [(ItemId, WorryLevel)]
newtype Operation = Operation { unOperation :: Expression Int}
newtype WorryDivisibleBy = WorryDivisibleBy { unWorryDivisbleBy :: Int}
newtype IfDivisibleTarget = IfDivisibleTarget { unIfDivisibleTarget :: MonkeyId }
newtype IfNotDivisibleTarget = IfNotDivisibleTarget { unIfNotDivisibleTarget :: MonkeyId}

data OperationParseError = UnexpectedInput String | MathExpressionParseError ExpressionParseError
                         deriving (Show)

data Day11ParseError = UnexpectedLineCountForMonkey [String]
                     | FailedToParseMonkeyId String
                     | FailedToParseStartingItems String
                     | FailedToParseOperation OperationParseError
                     | FailedToParseWorryDivisibleBy String
                     | FailedToParseIfDivisibleTarget String
                     | FailedToParseIfNotDivisibleTarget String
                     | NoInput
                     | MultipleMonkeysWithSameId MonkeyId
                     | InvalidTossTarget MonkeyId
                     deriving (Show)

data MonkeyStartParameters = MonkeyStartParameters MonkeyId StartingItems Operation WorryDivisibleBy IfDivisibleTarget IfNotDivisibleTarget
newtype Day11Input = Day11Input [MonkeyStartParameters]

parseDay11Input :: String -> Either Day11ParseError Day11Input
parseDay11Input = validateDay11Input <=< mapM tryParseMonkey . groupStrings
  where groupStrings :: String -> [[String]]
        groupStrings = splitAtEmpty . lines
        splitAtEmpty :: [String] -> [[String]]
        splitAtEmpty = foldr concatOrAppend []
        concatOrAppend :: String -> [[String]] -> [[String]]
        concatOrAppend "" rs = [] : rs
        concatOrAppend n (r:rs) = (n : r) : rs
        concatOrAppend n [] = [[n]]

tryParseMonkey :: [String] -> Either Day11ParseError MonkeyStartParameters
tryParseMonkey [l1,l2,l3,l4,l5,l6] =
  tryParseMonkeyId l1
  >>= tryParseInitialItems l2
  >>= tryParseOperation l3
  >>= tryParseWorryDivisibleBy l4
  >>= tryParseIfDivisibleTarget l5
  >>= tryParseNotIfDivisibleTarget l6
  where tryParseMonkeyId :: String -> Either Day11ParseError MonkeyId -- Returns MonkeyId, becasue tryParseInitialItems needs it to make itemIds.
        tryParseMonkeyId s | last s /= ':' = Left $ FailedToParseMonkeyId s
        tryParseMonkeyId ('M':'o':'n':'k':'e':'y':' ':s) = maybe (Left $ FailedToParseMonkeyId ("Monkey: " ++ s)) (Right . MonkeyId) $ readMaybe (init s)
        tryParseMonkeyId s = Left $ FailedToParseMonkeyId s
        tryParseInitialItems :: String -> MonkeyId -> Either Day11ParseError (Operation -> WorryDivisibleBy -> IfDivisibleTarget -> IfNotDivisibleTarget -> MonkeyStartParameters)
        tryParseInitialItems (' ':' ':'S':'t':'a':'r':'t':'i':'n':'g':' ':'i':'t':'e':'m':'s':':':' ':ss) m = 
          maybe
            (Left $ FailedToParseStartingItems $ "  Starting items: "++ss)
            (Right . MonkeyStartParameters m . convertWorryIntsToStartingItems m) (readMaybe (('[':ss) ++ "]"))
              where convertWorryIntsToStartingItems :: MonkeyId -> [Int] -> StartingItems
                    convertWorryIntsToStartingItems m = StartingItems . zipWith (\a b -> (ItemId (m,a), WorryLevel b)) [0..]
        tryParseInitialItems s _ = Left $ FailedToParseStartingItems s
        tryParseOperation :: String -> (Operation -> WorryDivisibleBy -> IfDivisibleTarget -> IfNotDivisibleTarget -> MonkeyStartParameters) -> Either Day11ParseError (WorryDivisibleBy -> IfDivisibleTarget -> IfNotDivisibleTarget -> MonkeyStartParameters)
        tryParseOperation (' ':' ':'O':'p':'e':'r':'a':'t':'i':'o':'n':':':' ':'n':'e':'w':' ':'=':' ':ss) ms =
          bimap (FailedToParseOperation . MathExpressionParseError) (ms . Operation) $ tryParseExpression (VariableNameValidator ("old" ==)) ss
        tryParseOperation s _ = Left $ FailedToParseOperation $ UnexpectedInput s
        tryParseWorryDivisibleBy :: String -> (WorryDivisibleBy -> IfDivisibleTarget -> IfNotDivisibleTarget -> MonkeyStartParameters) -> Either Day11ParseError (IfDivisibleTarget -> IfNotDivisibleTarget -> MonkeyStartParameters)
        tryParseWorryDivisibleBy (' ':' ':'T':'e':'s':'t':':':' ':'d':'i':'v':'i':'s':'i':'b':'l':'e':' ':'b':'y':' ':ss) ms = 
          maybe
            (Left $ FailedToParseWorryDivisibleBy $ "  Test: divisible by " ++ ss)
            (Right . ms . WorryDivisibleBy) (readMaybe ss)
        tryParseWorryDivisibleBy s _ = Left $ FailedToParseWorryDivisibleBy s
        tryParseIfDivisibleTarget :: String -> (IfDivisibleTarget -> IfNotDivisibleTarget -> MonkeyStartParameters) -> Either Day11ParseError (IfNotDivisibleTarget -> MonkeyStartParameters)
        tryParseIfDivisibleTarget (' ':' ':' ':' ':'I':'f':' ':'t':'r':'u':'e':':':' ':'t':'h':'r':'o':'w':' ':'t':'o':' ':'m':'o':'n':'k':'e':'y':' ':ss) ms =
          maybe
            (Left $ FailedToParseIfDivisibleTarget $ "    If true: throw to monkey " ++ ss)
            (Right . ms . IfDivisibleTarget . MonkeyId) (readMaybe ss)
        tryParseIfDivisibleTarget s _ = Left $ FailedToParseIfDivisibleTarget s
        tryParseNotIfDivisibleTarget :: String -> (IfNotDivisibleTarget -> MonkeyStartParameters) -> Either Day11ParseError MonkeyStartParameters
        tryParseNotIfDivisibleTarget (' ':' ':' ':' ':'I':'f':' ':'f':'a':'l':'s':'e':':':' ':'t':'h':'r':'o':'w':' ':'t':'o':' ':'m':'o':'n':'k':'e':'y':' ':ss) ms =
          maybe
            (Left $ FailedToParseIfNotDivisibleTarget $ "    If false: throw to monkey " ++ ss)
            (Right . ms . IfNotDivisibleTarget . MonkeyId) (readMaybe ss)
        tryParseNotIfDivisibleTarget s _ = Left $ FailedToParseIfNotDivisibleTarget s
tryParseMonkey s = Left $ UnexpectedLineCountForMonkey s

-- Checks if each MonkeyId is unique, and if each Target points to an existing MonkeyId
validateDay11Input :: [MonkeyStartParameters] -> Either Day11ParseError Day11Input
validateDay11Input p = if null p then Left NoInput else maybe 
  (maybe (Right (Day11Input p)) (Left . InvalidTossTarget) $ findFirstNotContainedItem tossTargets monkeyIds)
  (Left . MultipleMonkeysWithSameId)
  $ findFirstDuplicate monkeyIds
  where monkeyIds :: [MonkeyId]
        monkeyIds = map getMonkeyIdFromStartParam p
        tossTargets :: [MonkeyId]
        tossTargets = concatMap getTossTargets p
        getTossTargets :: MonkeyStartParameters -> [MonkeyId]
        getTossTargets (MonkeyStartParameters _ _ _ _ (IfDivisibleTarget a) (IfNotDivisibleTarget b)) = [a,b]

getMonkeyIdFromStartParam :: MonkeyStartParameters -> MonkeyId
getMonkeyIdFromStartParam (MonkeyStartParameters id _ _ _ _ _) = id

findFirstDuplicate :: (Eq a) => [a] -> Maybe a
findFirstDuplicate (a:as) = if a `elem` as then Just a else findFirstDuplicate as
findFirstDuplicate [] = Nothing

findFirstNotContainedItem :: (Eq a) => [a] -> [a] -> Maybe a
findFirstNotContainedItem (a:as) l = if a `notElem` l then Just a else findFirstNotContainedItem as l
findFirstNotContainedItem [] _ = Nothing

solveDay11Parts :: Day11Input -> (Int, ())
solveDay11Parts = bimap solveDay11Part1 (const ()) . (\x -> (x,x))

-- The input data is rather useless for iteration.
-- I think a quite acceptable (but still not ideal) representation is to factor out the data of each item (id, position, worry level), because that's
-- the only thing that changes. That way, we can just map over all items each turn.

data StaticMonkeyData = StaticMonkeyData { -- I was thinking about storing the function to get the target monkey directly in here, but I have no clue what part 2 wants...
  getMonkeyOperation ::Operation,
  getMonkeyWorryDivisbleBy :: WorryDivisibleBy,
  getMonkeyIfDivisibleTarget :: IfDivisibleTarget,
  getMonkeyIfNotDivisibleTarget :: IfNotDivisibleTarget
}

newtype Monkeys = Monkeys [(MonkeyId, StaticMonkeyData)]

makeMonkeysFromInput :: Day11Input -> Monkeys
makeMonkeysFromInput (Day11Input msp) = Monkeys $ map (\(MonkeyStartParameters id _ op wdb idt indt) -> (id,StaticMonkeyData op wdb idt indt)) msp

getMonkey :: Monkeys -> MonkeyId -> StaticMonkeyData -- no need to make this fallible. Our input data is validated, there's no way an invalid MonkeyId can exist.
getMonkey (Monkeys ms) m = fromJust $ lookup m ms

newtype ItemState = ItemState (MonkeyId, WorryLevel)
newtype ItemData = ItemData (ItemId, ItemState)
newtype Items = Items [ItemData]

getMonkeyIdHoldingItem :: ItemData -> MonkeyId
getMonkeyIdHoldingItem (ItemData (_, ItemState (m,_))) = m

isMonkeyHoldingItem :: MonkeyId -> ItemData -> Bool
isMonkeyHoldingItem mid id = mid == getMonkeyIdHoldingItem id

getTargetMonkeyForWorryLevel :: StaticMonkeyData -> WorryLevel -> MonkeyId
getTargetMonkeyForWorryLevel m w = 
  if 0 == (unWorryLevel w `rem` unWorryDivisbleBy (getMonkeyWorryDivisbleBy m)) then
    unIfDivisibleTarget $ getMonkeyIfDivisibleTarget m
  else
    unIfNotDivisibleTarget $ getMonkeyIfNotDivisibleTarget m

getWorryLevelAfterInspectItem :: StaticMonkeyData -> WorryLevel -> WorryLevel
getWorryLevelAfterInspectItem m iwl = 
  WorryLevel $ (`div` 3) . fromJust $ --fromJust is OK, we validated variable names when parsing input, and only "old" is allowed.
  evaluateIntExpression (unOperation $ getMonkeyOperation m) (VariableValues [(Variable "old", unWorryLevel iwl)]) 

makeItemsFromInput :: Day11Input -> Items
makeItemsFromInput (Day11Input msp) = 
  Items $ concatMap (\(MonkeyStartParameters mId (StartingItems is) _ _ _ _) -> map (\(iId, wl) -> ItemData (iId, ItemState (mId, wl))) is) msp

newtype Turns = Turns [MonkeyId]

makeTurnsFromInput :: Day11Input -> Turns
makeTurnsFromInput (Day11Input s) = Turns $ cycle $ map getMonkeyIdFromStartParam s
nextTurn :: Turns -> Turns
nextTurn (Turns t) = Turns $ tail t
currentTurn :: Turns -> MonkeyId
currentTurn (Turns t) = head t

-- Game is the state of the game before each turn.
data Game = Game Monkeys Items Turns
makeGameFromInput :: Day11Input -> Game
makeGameFromInput i = Game (makeMonkeysFromInput i) (makeItemsFromInput i) (makeTurnsFromInput i)

-- To get the state after a turn, we apply makeTurn on game. To get a list of all states after all turns, we can iterate over makeTurn.
makeTurn :: Game -> Game
makeTurn (Game m i t) = Game m (throwItems m i $ currentTurn t) $ nextTurn t

throwItems :: Monkeys -> Items -> MonkeyId -> Items
throwItems m (Items i) mid = Items $ map (conditionallyThrowItem (getMonkey m mid) mid) i
  where conditionallyThrowItem :: StaticMonkeyData -> MonkeyId -> ItemData -> ItemData
        conditionallyThrowItem _ mid i | not $ isMonkeyHoldingItem mid i = i
        conditionallyThrowItem m mid (ItemData (iid, ItemState (mhi, iwl))) = ItemData (iid, ItemState (getTargetMonkeyForWorryLevel m owl, owl))
          where owl = getWorryLevelAfterInspectItem m iwl

-- Needed later, to stop iteration
getTurnsPerRoundFromGame :: Game -> Int
getTurnsPerRoundFromGame (Game (Monkeys ms) _ _) = length ms -- cannot use turns here, turns is an infinite list... But monkeys take turns, sooo...

solveDay11Part1 :: Day11Input -> Int
solveDay11Part1 i = monkeyBusiness $ take twentyRounds $ iterate makeTurn game
  where game :: Game
        game = makeGameFromInput i
        twentyRounds :: Int
        twentyRounds = 20 * getTurnsPerRoundFromGame game

monkeyBusiness :: [Game] -> Int
monkeyBusiness = mulTwoBusiest . foldl countItemsPerActiveMonkey []
  where mulTwoBusiest :: [(MonkeyId, Int)] -> Int
        mulTwoBusiest = uncurry (*) . findLargestTwo
        findLargestTwo :: [(MonkeyId, Int)] -> (Int,Int)
        findLargestTwo = foldl (\(l,sl) (_,n) -> if n > l then (n, l) else (if n > sl then (l,n) else (l,sl))) (0,0)
        countItemsPerActiveMonkey :: [(MonkeyId, Int)] -> Game -> [(MonkeyId, Int)]
        countItemsPerActiveMonkey r (Game _ (Items is) t) = foldl (\o i -> if currentTurn t == getMonkeyIdHoldingItem i then incrementCountForMonkey o $ getMonkeyIdHoldingItem i else o) r is
          where incrementCountForMonkey :: [(MonkeyId, Int)] -> MonkeyId -> [(MonkeyId, Int)]
                incrementCountForMonkey [] mid = [(mid,1)]
                incrementCountForMonkey ((m, i):as) mid | m == mid = (m,i+1):as
                incrementCountForMonkey (a:as) mid = a:incrementCountForMonkey as mid


-- Part 2... Is a hack. It relies on the fact that all tests done by monkeys are "if foo divisible by bar", and that monkeys can only add and multiply.
-- For addition this is true: (a%c + b%c)%c = (a+b)%c
-- For multiplication this is true: ((a%c) * (b%c))%c = (a*b)%c
-- The hack part is, that the same trick does not work for division...