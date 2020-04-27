module Test.Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         
import           Control.Applicative ((<|>))
import           Expr                (evaluate, parseExpr,
                                      parseNum, parseOp)
import           Test.Helper
import           Test.Tasty.HUnit    (Assertion, assertBool, (@?=))
import           UberExpr            (Associativity (..), OpType (..), uberExpr)

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
    evaluate "1-2*3+4" @?= Just (1-2*3+4)
    evaluate "1-2-3" @?= Just (1-2-3)
    evaluate "4/2-2" @?= Just (4 `div` 2 - 2)
    evaluate "(1+2)*(3+4)" @?= Just ((1+2)*(3+4))
    evaluate "12+(23*(34)+456)" @?= Just (12+(23*(34)+456))
    evaluate "((1-(2*3))+4)" @?= Just ((1-(2*3))+4)
    evaluate "1-2+3-4" @?= Just (1-2+3-4)
    evaluate "6/2*3" @?= Just (6 `div` 2 * 3)

unit_parseNum :: Assertion
unit_parseNum = do
    testSuccess (runParser parseNum "7") (toStream "" 1) (7)
    testSuccess (runParser parseNum "12+3") (toStream "+3" 2) (12)
    testSuccess (runParser parseNum "007") (toStream "" 3) (7)
    testFailure (runParser parseNum "+3")
    testFailure (runParser parseNum "a")

unit_parseIdent :: Assertion
unit_parseIdent = do
    testSuccess (runParser parseIdent "abc def") (toStream " def" 3) "abc"
    testSuccess (runParser parseIdent "AbC dEf") (toStream " dEf" 3) "AbC"
    testSuccess (runParser parseIdent "_123") (toStream "" 4) "_123"
    testSuccess (runParser parseIdent "a_b_c d_e") (toStream " d_e" 5) "a_b_c"
    testSuccess (runParser parseIdent "x_ ") (toStream " " 2) "x_"
    testSuccess (runParser parseIdent "abc123") (toStream "" 6) "abc123"
    testSuccess (runParser parseIdent "_") (toStream "" 1) "_"
    testSuccess (runParser parseIdent "abc*1") (toStream "*1" 3) "abc"
    testFailure $ runParser parseIdent "123abc"
    testFailure $ runParser parseIdent "123"
    testFailure $ runParser parseIdent ""

unit_parseOp :: Assertion
unit_parseOp = do
    testSuccess (runParser parseOp "+1") (toStream "1" 1) Plus
    testSuccess (runParser parseOp "**") (toStream "*" 1) Mult
    testSuccess (runParser parseOp "-2") (toStream "2" 1) Minus
    testSuccess (runParser parseOp "/1") (toStream "1" 1) Div
    testFailure (runParser parseOp "12")

unit_parseExpr :: Assertion
unit_parseExpr = do
    testSuccess (runParser parseExpr "1*2*3"  ) (toStream "" 5) (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
    testSuccess (runParser parseExpr "123"    ) (toStream "" 3) (Num 123)
    testSuccess (runParser parseExpr "abc"    ) (toStream "" 3) (Ident "abc")
    testSuccess (runParser parseExpr "1*2+3*4") (toStream "" 7) (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
    testSuccess (runParser parseExpr "1+2*3+4") (toStream "" 7) (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3))) (Num 4))
    testSuccess (runParser parseExpr "1*x*3"  ) (toStream "" 5) (BinOp Mult (BinOp Mult (Num 1) (Ident "x")) (Num 3))
    testSuccess (runParser parseExpr "xyz"    ) (toStream "" 3) (Ident "xyz")
    testSuccess (runParser parseExpr "1*x+z*4") (toStream "" 7) (BinOp Plus (BinOp Mult (Num 1) (Ident "x")) (BinOp Mult (Ident "z") (Num 4)))
    testSuccess (runParser parseExpr "1+y*3+z") (toStream "" 7) (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Ident "y") (Num 3))) (Ident "z"))
    testSuccess (runParser parseExpr "1+x") (toStream "" 3) (BinOp Plus (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1-x") (toStream "" 3) (BinOp Minus (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1*x") (toStream "" 3) (BinOp Mult (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1/x") (toStream "" 3) (BinOp Div (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1^x") (toStream "" 3) (BinOp Pow (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1==x") (toStream "" 4)  (BinOp Equal (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1/=x") (toStream "" 4)  (BinOp Nequal (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1>x") (toStream "" 3) (BinOp Gt (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1>=x") (toStream "" 4)  (BinOp Ge (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1<x") (toStream "" 3) (BinOp Lt (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1<=x") (toStream "" 4)  (BinOp Le (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1&&x") (toStream "" 4)  (BinOp And (Num 1) (Ident "x"))
    testSuccess (runParser parseExpr "1||x") (toStream "" 4)  (BinOp Or (Num 1) (Ident "x"))
    -- (erasePosition $ runParser parseExpr "(1==x+2)||3*4<y-5/6&&(7/=z^8)||(id>12)&&abc<=13||xyz>=42") @?=
      -- (erasePosition $ runParser parseExpr "(1==(x+2))||(((3*4)<(y-(5/6))&&(7/=(z^8)))||(((id>12)&&(abc<=13))||(xyz>=42)))")

    testSuccess (runParser parseExpr "-1+2") (toStream "" 4) (BinOp Plus (UnaryOp Minus (Num 1)) (Num 2))
    testSuccess (runParser parseExpr "-1*2") (toStream "" 4) (BinOp Mult (UnaryOp Minus (Num 1)) (Num 2))
    testSuccess (runParser parseExpr "-1==2") (toStream "" 5) (BinOp Equal (UnaryOp Minus (Num 1)) (Num 2))
    testSuccess (runParser parseExpr "-1==-2") (toStream "" 6) (BinOp Equal (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    testSuccess (runParser parseExpr "-1&&-2") (toStream "" 6) (BinOp And (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    testSuccess (runParser parseExpr "!1&&!2") (toStream "" 6) (BinOp And (UnaryOp Not (Num 1)) (UnaryOp Not (Num 2)))
    testSuccess (runParser parseExpr "-1^2") (toStream "" 4) (UnaryOp Minus (BinOp Pow (Num 1) (Num 2)))
    testSuccess (runParser parseExpr "-1^(-2)") (toStream "" 7) (UnaryOp Minus (BinOp Pow (Num 1) (UnaryOp Minus (Num 2))))
    testSuccess (runParser parseExpr "(-1)^2") (toStream "" 6) (BinOp Pow (UnaryOp Minus (Num 1)) (Num 2))
    testSuccess (runParser parseExpr "-1+-2") (toStream "" 5) (BinOp Plus (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    testSuccess (runParser parseExpr "!-1") (toStream "" 3) (UnaryOp Not (UnaryOp Minus (Num 1)))
    testSuccess (runParser parseExpr "!(-1)") (toStream "" 5) (UnaryOp Not (UnaryOp Minus (Num 1)))
    testSuccess (runParser parseExpr "-(!1)") (toStream "" 5) (UnaryOp Minus (UnaryOp Not (Num 1)))
    testSuccess (runParser parseExpr "-1---2") (toStream "---2" 2) (UnaryOp Minus (Num 1))
    testSuccess (runParser parseExpr "-1^-2") (toStream "^-2" 2) (UnaryOp Minus (Num 1))

    testFailure $ runParser parseExpr "--1"
    testFailure $ runParser parseExpr "-!1"
