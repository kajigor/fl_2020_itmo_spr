module Test.DigitSum where 

import Test.HUnit (Assertion, (@?=))
import DigitSum (evaluate, splitOn)

unit_evaluation :: Assertion 
unit_evaluation = do 
    evaluate "1+2" @?= 3
    evaluate "2+4+8" @?= 14

unit_splitOn :: Assertion 
unit_splitOn = do 
    splitOn '+' "" @?= []
    splitOn '+' "1" @?= ['1']
    splitOn '+' "1+2+3" @?= ['1', '2', '3']