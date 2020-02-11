module Test.Polish where 

import Test.HUnit (Assertion, (@?=))
import Polish (evaluate)

unit_evaluation :: Assertion 
unit_evaluation = do 
    evaluate "12+" @?= 3
    evaluate "94-1+" @?= 6 
    evaluate "123*+" @?= 7