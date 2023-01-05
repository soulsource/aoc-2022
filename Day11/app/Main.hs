module Main where

import MonkeyBrain (evaluateIntExpression, tryParseExpression, Expression, VariableNameValidator (..), VariableValues (..), Variable (..))
import Data.Maybe (fromJust)

main :: IO ()
main = putStrLn "Hello, Haskell!"