module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import MonkeyBrain 

main = defaultMain tests

testMulDivPrecedence1 = TestCase $ assertEqual "(2+7*3+2-1-3+17/4*2)*(3+4*-1+20)/(17-3*4)"
    (Right (Just (((2+7*3+2-1-3+17 `quot` 4*2)*(3+4*(-1)+20) `quot` (17-3*4)) :: Int)))
    $ fmap (\x -> evaluateIntExpression x (VariableValues [])) (tryParseExpression (VariableNameValidator (const True)) "(2+7*3+2-1-3+17/4*2)*(3+4*-1+20)/(17-3*4)")

testSubDivChain1 = TestCase $ assertEqual "2+3-1+4"
    (Right (Just ((2+3-1+4) :: Int)))
    $ fmap (\x -> evaluateIntExpression x (VariableValues [])) (tryParseExpression (VariableNameValidator (const True)) "2+3-1+4")

testABitMoreComplex1 = TestCase $ assertEqual "17+37*2-(15-7/2*(5-1))*2+-(32-4*3)/5*2+33"
    (Right (Just ((17+37*2-(15-7 `quot` 2*(5-1))*2+(-(32-4*3)) `quot` 5*2+33) :: Int)))
    $ fmap (\x -> evaluateIntExpression x (VariableValues [])) (tryParseExpression (VariableNameValidator (const True)) "17+37*2-(15-7/2*(5-1))*2+-(32-4*3)/5*2+33")

testWithVariables = TestCase $ assertEqual "12+8*-a+37/b-(5-c*3), a=2, b=8, c=-1"
    (Right (Just ((12+8*(-2)+37 `quot` 8-(5-(-1)*3)) :: Int)))
    $ fmap (\x -> evaluateIntExpression x (VariableValues [(Variable "a", 2), (Variable "b", 8), (Variable "c", -1)]))
           (tryParseExpression (VariableNameValidator (const True)) "12+8*-a+37/b-(5-c*3)")

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "Operator Precedence, Mul, Div 1" testMulDivPrecedence1,
                                     TestLabel "Simple Addition Subtraction Chain" testSubDivChain1,
                                     TestLabel "A bit more complex formula" testABitMoreComplex1,
                                     TestLabel "With three Variables" testWithVariables]