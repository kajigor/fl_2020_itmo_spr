module Test.Expr where

import           AST              (AST (..), Operator (..))
import           Combinators      (Result (..), runParser)
import           Expr             
import           Test.Tasty.HUnit (Assertion, (@?=), assertBool)

isFailure (Failure _) = True
isFailure  _          = False

unit_evaluate :: Assertion
unit_evaluate = do
    evaluate "1" @?= Just 1
    evaluate "1+2" @?= Just (1+2)
    evaluate "2+4+8" @?= Just (2+4+8)
    evaluate "11+22" @?= Just (11+22)
    evaluate "13+42+777" @?= Just (13+42+777)
    evaluate "31+24+777" @?= Just (31+24+777)
    evaluate "1+2*3+4" @?= Just (1+2*3+4)
    evaluate "12+23*34+456" @?= Just (12+23*34+456)
    --evaluate "1-2*3+4" @?= Just (1-(2*3+4))
   -- evaluate "1-2-3" @?= Just (1-(2-3))
    evaluate "4/2-2" @?= Just ((4 `div` 2) - 2)
    evaluate "(1+2)*(3+4)" @?= Just ((1+2)*(3+4))
    evaluate "12+(23*(34)+456)" @?= Just (12+(23*(34)+456))
    evaluate "((1-(2*3))+4)" @?= Just ((1-(2*3))+4)
    evaluate "1-2+3-4" @?= Just (1-2+3-4)
    evaluate "6/2*3" @?= Just ((6 `div` 2) * 3)

unit_parseNum :: Assertion
unit_parseNum = do
    runParser parseNum "7" @?= Success "" 7
    runParser parseNum "12+3" @?= Success "+3" 12
    runParser parseNum "007" @?= Success "" 7
    isFailure (runParser parseNum "+3") @?= True
    isFailure (runParser parseNum "a") @?= True

unit_parseNegNum :: Assertion
unit_parseNegNum = do
    runParser parseNum "123" @?= Success "" 123
    runParser parseNum "-123" @?= Success "" (-123)
    runParser parseNum "--123" @?= Success "" 123
    assertBool "" $ isFailure $ runParser parseNum "+-3"
    assertBool "" $ isFailure $ runParser parseNum "-+3"
    assertBool "" $ isFailure $ runParser parseNum "-a"

unit_parseIdent :: Assertion
unit_parseIdent = do
    runParser parseIdent "abc def" @?= Success " def" "abc"
    runParser parseIdent "AbC dEf" @?= Success " dEf" "AbC"
    runParser parseIdent "_123" @?= Success "" "_123"
    runParser parseIdent "a_b_c d_e" @?= Success " d_e" "a_b_c"
    runParser parseIdent "x_ " @?= Success " " "x_"
    runParser parseIdent "abc123" @?= Success "" "abc123"
    runParser parseIdent "_" @?= Success "" "_"
    runParser parseIdent "abc*1" @?= Success "*1" "abc"
    assertBool "" $ isFailure $ runParser parseIdent "123abc"
    assertBool "" $ isFailure $ runParser parseIdent "123"
    assertBool "" $ isFailure $ runParser parseIdent ""


unit_parseOp :: Assertion
unit_parseOp = do
    runParser parseOp "+1" @?= Success "1" Plus
    runParser parseOp "**" @?= Success "*" Mult
    runParser parseOp "-2" @?= Success "2" Minus
    runParser parseOp "/1" @?= Success "1" Div
    isFailure (runParser parseOp "12") @?= True

unit_parseExpr :: Assertion
unit_parseExpr = do
    runParser parseExpr "1*2*3"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
    runParser parseExpr "123"     @?= Success "" (Num 123)
    runParser parseExpr "abc"     @?= Success "" (Ident "abc")
    runParser parseExpr "1*2+3*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
    runParser parseExpr "1+2*3+4" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3))) (Num 4))
    runParser parseExpr "1*x*3"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Ident "x")) (Num 3))
    runParser parseExpr "xyz"     @?= Success "" (Ident "xyz")
    runParser parseExpr "1*x+z*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Ident "x")) (BinOp Mult (Ident "z") (Num 4)))
    runParser parseExpr "1+y*3+z" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Ident "y") (Num 3))) (Ident "z"))
    runParser parseExpr "1+x" @?= Success "" (BinOp Plus (Num 1) (Ident "x"))
    runParser parseExpr "1-x" @?= Success "" (BinOp Minus (Num 1) (Ident "x"))
    runParser parseExpr "1*x" @?= Success "" (BinOp Mult (Num 1) (Ident "x"))
    runParser parseExpr "1/x" @?= Success "" (BinOp Div (Num 1) (Ident "x"))
    runParser parseExpr "1^x" @?= Success "" (BinOp Pow (Num 1) (Ident "x"))
    runParser parseExpr "1==x" @?= Success "" (BinOp Equal (Num 1) (Ident "x"))
    runParser parseExpr "1/=x" @?= Success "" (BinOp Nequal (Num 1) (Ident "x"))
    runParser parseExpr "1>x" @?= Success "" (BinOp Gt (Num 1) (Ident "x"))
    runParser parseExpr "1>=x" @?= Success "" (BinOp Ge (Num 1) (Ident "x"))
    runParser parseExpr "1<x" @?= Success "" (BinOp Lt (Num 1) (Ident "x"))
    runParser parseExpr "1<=x" @?= Success "" (BinOp Le (Num 1) (Ident "x"))
    runParser parseExpr "1&&x" @?= Success "" (BinOp And (Num 1) (Ident "x"))
    runParser parseExpr "1||x" @?= Success "" (BinOp Or (Num 1) (Ident "x"))
    runParser parseExpr "(1==x+2)||3*4<y-5/6&&(7/=z^8)||(id>12)&&abc<=13||xyz>=42" @?=
      runParser parseExpr "(1==(x+2))||(((3*4)<(y-(5/6))&&(7/=(z^8)))||(((id>12)&&(abc<=13))||(xyz>=42)))"

unit_parseExpr' :: Assertion
unit_parseExpr' = do
  isFailure (runParser parseExpr "-1>3") @?= False
  isFailure (runParser parseExpr "--1>3") @?= False
  isFailure (runParser parseExpr "--1+_a+c+d+23+--15*-29") @?= False



unit_parseMult :: Assertion
unit_parseMult = do
    --runParser parseMult "1*2*3" @?= Success "" (BinOp Mult (Num 1) (BinOp Mult (Num 2) (Num 3)))
    runParser parseMult "123" @?= Success "" (Num 123)
    runParser parseMult "1*2+3*4" @?= Success "+3*4" (BinOp Mult (Num 1) (Num 2))

unit_parseSum :: Assertion
unit_parseSum = do
    --runParser parseSum "1*2*3"   @?= Success "" (BinOp Mult (Num 1) (BinOp Mult (Num 2) (Num 3)))
    runParser parseSum "123"     @?= Success "" (Num 123)
    runParser parseSum "1*2+3*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
    --runParser parseSum "1+2*3+4" @?= Success "" (BinOp Plus (Num 1) (BinOp Plus (BinOp Mult (Num 2) (Num 3)) (Num 4)))
