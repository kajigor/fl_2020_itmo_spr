module Test.Expr where

import           AST              (AST (..), Operator (..))
import           Combinators      (Result (..), runParser)
import           Expr             (evaluate, parseMult, parseNum, parseOp,
                                   parseSum)
import           Test.Tasty.HUnit (Assertion, (@?=))

isFailure (Failure _) = True
isFailure  _          = False

unit_evaluate :: Assertion
unit_evaluate = do
  evaluate "1" @?= Just 1
  evaluate "1+2" @?= Just (1 + 2)
  evaluate "2+4+8" @?= Just (2 + 4 + 8)
  evaluate "11+22" @?= Just (11 + 22)
  evaluate "13+42+777" @?= Just (13 + 42 + 777)
  evaluate "31+24+777" @?= Just (31 + 24 + 777)
  evaluate "1+2*3+4" @?= Just (1 + 2 * 3 + 4)
  evaluate "12+23*34+456" @?= Just (12 + 23 * 34 + 456)
  evaluate "1-2*3+4" @?= Just (1 - 2 * 3 + 4)
  evaluate "1-2-3" @?= Just (1 - 2 - 3)
  evaluate "4/2-2" @?= Just (4 `div` 2 - 2)
  evaluate "(1+2)*(3+4)" @?= Just ((1 + 2) * (3 + 4))
  evaluate "12+(23*(34)+456)" @?= Just (12 + (23 * 34 + 456))
  evaluate "((1-(2*3))+4)" @?= Just ((1 - (2 * 3)) + 4)
  evaluate "1-2+3-4" @?= Just (1 - 2 + 3 - 4)
  evaluate "6/2*3" @?= Just ((6 `div` 2) * 3)

unit_parseNum :: Assertion
unit_parseNum = do
    runParser parseNum "7" @?= Success "" 7
    runParser parseNum "12+3" @?= Success "+3" 12
    runParser parseNum "007" @?= Success "" 7
    isFailure (runParser parseNum "+3") @?= True
    isFailure (runParser parseNum "a") @?= True

unit_parseOp :: Assertion
unit_parseOp = do
    runParser parseOp "+1" @?= Success "1" Plus
    runParser parseOp "**" @?= Success "*" Mult
    runParser parseOp "-2" @?= Success "2" Minus
    runParser parseOp "/1" @?= Success "1" Div
    isFailure (runParser parseOp "12") @?= True

unit_parseMult :: Assertion
unit_parseMult = do
    runParser parseMult "1*2*3" @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
    runParser parseMult "123" @?= Success "" (Num 123)
    runParser parseMult "1*2+3*4" @?= Success "+3*4" (BinOp Mult (Num 1) (Num 2))

unit_parseSum :: Assertion
unit_parseSum = do
    runParser parseSum "1*2*3"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
    runParser parseSum "123"     @?= Success "" (Num 123)
    runParser parseSum "1*2+3*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
    runParser parseSum "1+2*3+4" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3))) (Num 4))
