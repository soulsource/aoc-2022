module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import MonkeyBrain 

main = defaultMain tests

test1 = TestCase $ assertEqual "(2+7*3+2-1-3+17/4*2)*(3+4*-1+20)/(17-3*4)"
    (Right (Just (((2+7*3+2-1-3+17 `quot` 4*2)*(3+4*(-1)+20) `quot` (17-3*4)) :: Int)))
    $ fmap (\x -> evaluateIntExpression x (VariableValues [])) (tryParseExpression (VariableNameValidator (const True)) "(2+7*3+2-1-3+17/4*2)*(3+4*-1+20)/(17-3*4)")

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "Test1" test1]