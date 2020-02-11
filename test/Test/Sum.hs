module Test.Sum where 

import Test.HUnit (Assertion, (@?=))
import Sum (evaluate, parseNum, splitOn)

unit_evaluation :: Assertion 
unit_evaluation = do 
    evaluate "1+2" @?= 3
    evaluate "2+4+8" @?= 14
    evaluate "11+22" @?= 33
    evaluate "13+42+777" @?= 832
    evaluate "31+24+777" @?= 832

unit_parseNum :: Assertion 
unit_parseNum = do 
    parseNum "1" @?= 1
    parseNum "42" @?= 42
    parseNum "123" @?= 123 

unit_splitOn :: Assertion 
unit_splitOn = do 
    splitOn '+' "" @?= [""]
    splitOn '+' "1" @?= ["1"]
    splitOn '+' "1+2+3" @?= ["1", "2", "3"]
    splitOn '+' "13+42+777" @?= ["13", "42", "777"]
