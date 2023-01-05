module MonkeyBrain (
    evaluateIntExpression,
    evaluateFracExpression,
    evaluateExpression,
    tryParseExpression,
    Expression,
    VariableNameValidator (..),
    VariableValues (..),
    Variable (..),
    ExpressionParseError(..),
    AddFn (..),
    SubFn (..),
    MulFn (..),
    DivFn (..),
    NegFn (..)
) where

import Control.Exception (assert)
import Data.Either (isRight)
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Text.Read (readMaybe)
import Control.Monad.Zip (MonadZip(mzip))

newtype VariableNameValidator = VariableNameValidator (String -> Bool)

data Expression a = Add (Expression a) (Expression a)
                  | Sub (Expression a) (Expression a)
                  | Mul (Expression a) (Expression a)
                  | Div (Expression a) (Expression a)
                  | Neg (Expression a)
                  | Value a
                  | Var Variable
                  deriving (Show)

newtype Variable = Variable String deriving (Eq, Show)

newtype VariableValues a = VariableValues [(Variable, a)] -- List, because, let's be honest: AOC Day11 just has one variable.

-- Evaluates the given expression with the supplied set of variables. Returns Nothing if not all required variables have a value.
evaluateIntExpression :: (Integral a) => Expression a -> VariableValues a -> Maybe a
evaluateIntExpression = evaluateExpression (AddFn (+)) (SubFn (-)) (MulFn (*)) (DivFn quot) (NegFn negate)

evaluateFracExpression :: (Fractional a) => Expression a -> VariableValues a -> Maybe a
evaluateFracExpression = evaluateExpression (AddFn (+)) (SubFn (-)) (MulFn (*)) (DivFn (/)) (NegFn negate)

newtype AddFn a = AddFn (a -> a -> a)
newtype SubFn a = SubFn (a -> a -> a)
newtype MulFn a = MulFn (a -> a -> a)
newtype DivFn a = DivFn (a -> a -> a)
newtype NegFn a = NegFn (a -> a)

evaluateExpression :: AddFn a -> SubFn a -> MulFn a -> DivFn a -> NegFn a -> Expression a -> VariableValues a -> Maybe a
evaluateExpression (AddFn add) s m d n (Add e1 e2) v = uncurry add <$> mzip (evaluateExpression (AddFn add) s m d n e1 v) (evaluateExpression (AddFn add) s m d n e2 v)
evaluateExpression a (SubFn sub) m d n (Sub e1 e2) v = uncurry sub <$> mzip (evaluateExpression a (SubFn sub) m d n e1 v) (evaluateExpression a (SubFn sub) m d n e2 v)
evaluateExpression a s (MulFn mul) d n (Mul e1 e2) v = uncurry mul <$> mzip (evaluateExpression a s (MulFn mul) d n e1 v) (evaluateExpression a s (MulFn mul) d n e2 v)
evaluateExpression a s m (DivFn div) n (Div e1 e2) v = uncurry div <$> mzip (evaluateExpression a s m (DivFn div) n e1 v) (evaluateExpression a s m (DivFn div) n e2 v)
evaluateExpression a s m d (NegFn neg) (Neg e) v = neg <$> evaluateExpression a s m d (NegFn neg) e v
evaluateExpression a s m d n (Value val) v = Just val
evaluateExpression a s m d n (Var var) (VariableValues vals) = lookup var vals

data ExpressionParseError = MismatchedBrackets
                          | VariableFailedValidation String
                          deriving (Show, Eq)

-- Tries to parse the input string as a maths expression. Returns Left in case of parse errors.
tryParseExpression :: (Read a) => VariableNameValidator -> String -> Either ExpressionParseError (Expression a)
tryParseExpression (VariableNameValidator v) = tryParse (VariableNameValidator (\vs -> v vs && internalValidator vs)) <=< (validateBracketPairing . removeAllWhitespace)
    where removeAllWhitespace = filter (' ' /=)
          

------------------------------------------------------------------------------------------------------
-- Private stuff below

newtype StringWithMatchingBrackets = StringWithMatchingBrackets {getStringWithMatchingBrackets :: String}

-- To be run exactly once on the input
validateBracketPairing :: String -> Either ExpressionParseError StringWithMatchingBrackets
validateBracketPairing s = if allBracketsMatch 0 s then Right (StringWithMatchingBrackets s) else Left MismatchedBrackets
    where allBracketsMatch :: Word -> String -> Bool
          allBracketsMatch i [] = i == 0
          allBracketsMatch 0 (')':_) = False
          allBracketsMatch i (')':ss) = allBracketsMatch (i-1) ss
          allBracketsMatch i ('(':ss) = allBracketsMatch (i+1) ss
          allBracketsMatch i (_:ss) = allBracketsMatch i ss
          

removeEnclosingBrackets :: StringWithMatchingBrackets -> StringWithMatchingBrackets
removeEnclosingBrackets (StringWithMatchingBrackets ('(':cs)) | isWholeStringInBrackets 1 cs = removeEnclosingBrackets $ StringWithMatchingBrackets $ init cs
    where isWholeStringInBrackets :: Word -> String -> Bool -- Since this is a local function, I think working on String directly is ok...
          isWholeStringInBrackets 0 (_:_) = False -- Reached top level of brackets, but string is not at end
          isWholeStringInBrackets 0 [] = True
          isWholeStringInBrackets i ('(':ss) = isWholeStringInBrackets (i+1) ss
          isWholeStringInBrackets i (')':ss) = isWholeStringInBrackets (i-1) ss
          isWholeStringInBrackets i (_:ss) = isWholeStringInBrackets i ss
removeEnclosingBrackets s = s

internalValidator :: String -> Bool
internalValidator st = not (null st) && all (\s -> not (s == '(' || s== ')' || s == '+' || s=='-' || s=='*' || s=='/')) st

tryParse :: (Read a) => VariableNameValidator -> StringWithMatchingBrackets -> Either ExpressionParseError (Expression a)
tryParse v (StringWithMatchingBrackets s) | assert (isRight $ validateBracketPairing s) False = undefined -- for debugging. Checks invariant of StringWithMatchingBrackets
tryParse v sr = tryMakeBinaryExpression v s
                >>= (\x -> (<|> tryReadValue s) <$> fmap (x <|>) (tryMakeUnaryExpression v s) )
                >>= maybe (tryReadVariable v s) Right
    where s = removeEnclosingBrackets sr

zipEithers :: Either a b -> Either a b -> Either a (b,b)
zipEithers (Left l) _ = Left l
zipEithers _ (Left l) = Left l
zipEithers (Right r1) (Right r2) = Right (r1,r2)

--tries to parse a +-*/ expression.
tryMakeBinaryExpression :: (Read a) => VariableNameValidator -> StringWithMatchingBrackets -> Either ExpressionParseError (Maybe (Expression a))
tryMakeBinaryExpression v (StringWithMatchingBrackets s) = mapM (makeBinaryExpression s) $ findLowestPriorityRightMostBinaryOperator s
    where makeBinaryExpression :: (Read a1) => String -> Int -> Either ExpressionParseError (Expression a1)
          makeBinaryExpression s i = makeOperator operator <$> zipEithers (recurse firstSubExpression) (recurse secondSubExpression)
            where (firstSubExpression, operator:secondSubExpression) = splitAt i s
                  recurse :: (Read a2) => String -> Either ExpressionParseError (Expression a2)
                  recurse = tryParse v . StringWithMatchingBrackets
                  makeOperator :: Char -> (Expression a, Expression a) -> Expression a
                  makeOperator '+' (a,b) = Add a b
                  makeOperator '-' (a,b) = Sub a b
                  makeOperator '*' (a,b) = Mul a b
                  makeOperator '/' (a,b) = Div a b
          findLowestPriorityRightMostBinaryOperator :: String -> Maybe Int
          findLowestPriorityRightMostBinaryOperator = fLpRmBoWorker Nothing . filterBrackets 0 . zip [0..]
            where filterBrackets :: Int -> [(Int, Char)] -> [(Int, Char)]
                  filterBrackets i ((_,'('):ss) = filterBrackets (i+1) ss
                  filterBrackets i ((_,')'):ss) = filterBrackets (i-1) ss
                  filterBrackets 0 (s:ss) = s:filterBrackets 0 ss
                  filterBrackets 0 [] = []
                  filterBrackets i (s:ss) = filterBrackets i ss

                  fLpRmBoWorker :: Maybe (Int, Char) -> [(Int, Char)] -> Maybe Int
                  fLpRmBoWorker f ((0,_):ss) = fLpRmBoWorker f ss -- first character cannot be a binary operator.
                  fLpRmBoWorker f [] = fst <$> f
                  fLpRmBoWorker f ((i,c):(j,_):ss) | isOperator c && hasSameOrLowerPrecedence (snd <$> f) c && j==i+1 = fLpRmBoWorker (Just (i,c)) ss -- skip the first character right after operator. If it's another operator, it's unary.
                  fLpRmBoWorker f ((i,c):(j,_):ss) | isOperator c && j==i+1 = fLpRmBoWorker f ss
                  fLpRmBoWorker f ((i,c):ss) | isOperator c && hasSameOrLowerPrecedence (snd <$> f) c = fLpRmBoWorker (Just (i,c)) ss
                  fLpRmBoWorker f (s:ss) = fLpRmBoWorker f ss -- no operator means just go to next char.

                  isOperator :: Char -> Bool
                  isOperator c = ('+' == c) || ('-' == c) || ('*' == c) || ('/' == c)

                  hasSameOrLowerPrecedence :: Maybe Char -> Char -> Bool -- this is not typesafe, but well, it's a local helper, so it's probably fine...
                  hasSameOrLowerPrecedence Nothing _ = True
                  hasSameOrLowerPrecedence (Just '/') _ = True
                  hasSameOrLowerPrecedence (Just '*') _ = True
                  hasSameOrLowerPrecedence (Just '-') c = c == '-' || c == '+'
                  hasSameOrLowerPrecedence (Just '+') c = c == '-' || c == '+'



--tries to make neg expression
tryMakeUnaryExpression :: (Read a) => VariableNameValidator -> StringWithMatchingBrackets -> Either ExpressionParseError (Maybe (Expression a))
tryMakeUnaryExpression v (StringWithMatchingBrackets ('-':ss)) = Just . Neg <$> tryParse v (StringWithMatchingBrackets ss)
tryMakeUnaryExpression _ _ = Right Nothing

--tries to readMaybe.
tryReadValue :: (Read a) => StringWithMatchingBrackets -> Maybe (Expression a)
tryReadValue = fmap Value . readMaybe . getStringWithMatchingBrackets

tryReadVariable :: VariableNameValidator -> StringWithMatchingBrackets -> Either ExpressionParseError (Expression a)
tryReadVariable (VariableNameValidator v) (StringWithMatchingBrackets s) | v s = Right $ Var (Variable s)
tryReadVariable _ (StringWithMatchingBrackets s) = Left $ VariableFailedValidation s