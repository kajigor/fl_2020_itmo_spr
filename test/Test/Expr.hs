module Test.Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (InputStream (..), Parser (..), Position (..),
                                      Result (..), runParser,
                                      symbol, toStream, word)
import           Control.Applicative ((<|>))
import           Test.Helper
import           Test.Tasty.HUnit    (Assertion, assertBool, (@?=))
import           UberExpr            (Associativity (..), OpType (..), uberExpr)
import           AST              (AST (..), Operator (..))
import           Expr             (evaluate, parseNum, parseOp,
                                   parseExpr, parseIdent, evalExpr)
import qualified Data.Map as Map

fromSuccess (Success _ _ ast) = ast

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
    evaluate "4/2-2" @?= Just ((4 `div` 2) - 2)
    evaluate "(1+2)*(3+4)" @?= Just ((1+2)*(3+4))
    evaluate "12+(23*(34)+456)" @?= Just (12+(23*(34)+456))
    evaluate "((1-(2*3))+4)" @?= Just ((1-(2*3))+4)
    evaluate "1-2+3-4" @?= Just (1-2+3-4)
    evaluate "6/2*3" @?= Just (6 `div` 2 * 3)

unit_parseNum :: Assertion
unit_parseNum = do
{-
    runParser parseNum "7" @?= Success "" 7
    runParser parseNum "12+3" @?= Success "+3" 12
    runParser parseNum "007" @?= Success "" 7
-}
    testFailure $ runParser parseNum "+3"
    testFailure $ runParser parseNum "a"

unit_parseNegNum :: Assertion
unit_parseNegNum = do
--    runParser parseNum "123" @?= Success (toStream "" 3) 123
--    runParser parseNum "-123" @?= Success (toStream "" 4) (-123)
    testFailure $ runParser parseNum "--123"
    testFailure $ runParser parseNum "+-3"
    testFailure $ runParser parseNum "-+3"
    testFailure $ runParser parseNum "-a"
    testSuccessErase (runParser parseNum "-  20") "" (-20)
    testFailure $ runParser parseNum "-  +20"
    testFailure $ runParser parseNum "- - 20"

unit_parseNegInExpr :: Assertion
unit_parseNegInExpr = do
    testSuccessErase (runParser parseExpr "-123+-10") "" (BinOp Plus (UnaryOp Minus $ Num 123) (UnaryOp Minus $ Num 10))
{-
    runParser parseExpr "(abc*-50)--20" @?= Success "" (BinOp Minus (BinOp Mult (Ident "abc") (UnaryOp Minus $ Num 50)) (UnaryOp Minus $ Num 20))
    runParser parseExpr "-(abc*20)" @?= Success "" (UnaryOp Minus (BinOp Mult (Ident "abc") (Num 20)))
    runParser parseExpr "12*---20" @?= Success "*---20" (Num 12)
    runParser parseExpr "12  *   -  20" @?= Success "" (BinOp Mult (Num 12) (UnaryOp Minus $ Num 20))
    runParser parseExpr "(  300 +  - 40)   ^  ( - 1)" @?= Success "" (BinOp Pow (BinOp Plus (Num 300) (UnaryOp Minus $ Num 40)) (UnaryOp Minus $ Num 1))
    runParser parseExpr "100 --   10--    12" @?= Success "" (BinOp Minus (BinOp Minus (Num 100) (UnaryOp Minus $ Num 10)) (UnaryOp Minus $ Num 12))
-}
unit_parseIdent :: Assertion
unit_parseIdent = do
{-
    runParser parseIdent "abc def" @?= Success " def" "abc"
    runParser parseIdent "AbC dEf" @?= Success " dEf" "AbC"
    runParser parseIdent "_123" @?= Success "" "_123"
    runParser parseIdent "a_b_c d_e" @?= Success " d_e" "a_b_c"
    runParser parseIdent "x_ " @?= Success " " "x_"
    runParser parseIdent "abc123" @?= Success "" "abc123"
    runParser parseIdent "_" @?= Success "" "_"
    runParser parseIdent "abc*1" @?= Success "*1" "abc"
-}
    testFailure $ runParser parseIdent "123abc"
    testFailure $ runParser parseIdent "123"
    testFailure $ runParser parseIdent ""

unit_parseOp :: Assertion
unit_parseOp = do
    testSuccess (runParser parseOp "+1") (toStream "1" $ Position 1 1) Plus
    testSuccess (runParser parseOp "**") (toStream "*" $ Position 1 1) Mult
    testSuccess (runParser parseOp "-2") (toStream "2" $ Position 1 1) Minus
    testSuccess (runParser parseOp "/1") (toStream "1" $ Position 1 1) Div
    testFailure (runParser parseOp "12")

unit_parseExpr :: Assertion
unit_parseExpr = do
    testSuccess (runParser parseExpr "1*2*3"  ) (toStream "" $ Position 1 5) (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
{-
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
-}
unit_ParseExprWithSpaces :: Assertion
unit_ParseExprWithSpaces = do
    testSuccessErase (runParser parseExpr "1 * 2  *  3") "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
    -- Произвольное число пробелов поддерживается только между операторами:
{-
    assertBool "" $ isFailure $ runParser parseExpr "  123   "
    assertBool "" $ isFailure $ runParser parseExpr "  abc"
    runParser parseExpr "1 *2  + 3*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
    runParser parseExpr "1 + 2 *3+    4" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3))) (Num 4))
    runParser parseExpr "1 * x * 3"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Ident "x")) (Num 3))
    runParser parseExpr "1  *x +z   *4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Ident "x")) (BinOp Mult (Ident "z") (Num 4)))
    runParser parseExpr "1+ y * 3+z" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Ident "y") (Num 3))) (Ident "z"))
    runParser parseExpr "1  +x" @?= Success "" (BinOp Plus (Num 1) (Ident "x"))
    runParser parseExpr "1-  x" @?= Success "" (BinOp Minus (Num 1) (Ident "x"))
    runParser parseExpr "1 * x" @?= Success "" (BinOp Mult (Num 1) (Ident "x"))
    runParser parseExpr "1  / x" @?= Success "" (BinOp Div (Num 1) (Ident "x"))
    runParser parseExpr "1  ^x" @?= Success "" (BinOp Pow (Num 1) (Ident "x"))
    runParser parseExpr "1   == x" @?= Success "" (BinOp Equal (Num 1) (Ident "x"))
    runParser parseExpr "1  /=x" @?= Success "" (BinOp Nequal (Num 1) (Ident "x"))
    runParser parseExpr "1>  x" @?= Success "" (BinOp Gt (Num 1) (Ident "x"))
    runParser parseExpr "1  >= x" @?= Success "" (BinOp Ge (Num 1) (Ident "x"))
    runParser parseExpr "1  <x" @?= Success "" (BinOp Lt (Num 1) (Ident "x"))
    runParser parseExpr "1<=  x" @?= Success "" (BinOp Le (Num 1) (Ident "x"))
    runParser parseExpr "1     &&   x" @?= Success "" (BinOp And (Num 1) (Ident "x"))
    runParser parseExpr "1||      x" @?= Success "" (BinOp Or (Num 1) (Ident "x"))
    runParser parseExpr "(    1  == x+2  )  ||  3* 4< y-5   /6&&  (7  /=  z^8)||(id>12)&&abc<=13||xyz>=42" @?=
      runParser parseExpr "(1  ==  (  x + 2  )  )||(((3*4) < (y-(5/6))  &&(7  /=  (z  ^  8)))||(((id>12)&&(abc<=13))||(xyz>=42)))"
-}

unit_unaryEpxr = do
    testSuccessErase (runParser parseExpr "-1+2") "" (BinOp Plus (UnaryOp Minus (Num 1)) (Num 2))
{-
    runParser parseExpr "-1*2" @?= Success "" (BinOp Mult (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr "-1==2" @?= Success "" (BinOp Equal (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr "-1==-2" @?= Success "" (BinOp Equal (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    runParser parseExpr "-1&&-2" @?= Success "" (BinOp And (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    runParser parseExpr "!1&&!2" @?= Success "" (BinOp And (UnaryOp Not (Num 1)) (UnaryOp Not (Num 2)))
    runParser parseExpr "-1^2" @?= Success "" (UnaryOp Minus (BinOp Pow (Num 1) (Num 2)))
    runParser parseExpr "-1^(-2)" @?= Success "" (UnaryOp Minus (BinOp Pow (Num 1) (UnaryOp Minus (Num 2))))
    runParser parseExpr "(-1)^2" @?= Success "" (BinOp Pow (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr "-1+-2" @?= Success "" (BinOp Plus (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    runParser parseExpr "!-1" @?= Success "" (UnaryOp Not (UnaryOp Minus (Num 1)))
    runParser parseExpr "!(-1)" @?= Success "" (UnaryOp Not (UnaryOp Minus (Num 1)))
    runParser parseExpr "-(!1)" @?= Success "" (UnaryOp Minus (UnaryOp Not (Num 1)))
    runParser parseExpr "-1---2" @?= Success "---2" (UnaryOp Minus (Num 1))

    runParser parseExpr "-1^-2" @?= Success "^-2" (UnaryOp Minus (Num 1))
    assertBool "" $ isFailure $ runParser parseExpr "--1"
    assertBool "" $ isFailure $ runParser parseExpr "-!1"
-}

unit_funcCalls = do
    testSuccess (runParser parseExpr "foo(x)") (toStream "" $ Position 1 6) (FunctionCall "foo" [Ident "x"])
    testSuccess (runParser parseExpr "bar()") (toStream "" $ Position 1 5) (FunctionCall "bar" [])
    testSuccessErase (runParser parseExpr "foo( x, z) * 200 + abc") ""
      (BinOp Plus (BinOp Mult (FunctionCall "foo" [Ident "x", Ident "z"]) (Num 200)) (Ident "abc"))
    -- Parsed as idents:
    testSuccess (runParser parseExpr "foo(x,)") (toStream "(x,)" $ Position 1 3) (Ident "foo")
    testSuccess (runParser parseExpr "200-foo(,x)") (toStream "(,x)" $ Position 1 7) (BinOp Minus (Num 200) (Ident "foo"))

unit_evalExpr = do
    evalExpr Map.empty (fromSuccess $ runParser parseExpr "-5") @?= Just (-5)
    evalExpr Map.empty (fromSuccess $ runParser parseExpr "3 * 7 + 1 - 2 * 2") @?= Just 18
    evalExpr Map.empty (fromSuccess $ runParser parseExpr "2 ^ 5 + 5 * 10") @?= Just 82
    evalExpr Map.empty (fromSuccess $ runParser parseExpr "- 2 ^ 3 + 4 == - 2 * 2") @?= Just 1
    evalExpr Map.empty (fromSuccess $ runParser parseExpr "2 == 5") @?= Just 0
    evalExpr Map.empty (fromSuccess $ runParser parseExpr "2 && 11 && 5") @?= Just 1
    evalExpr Map.empty (fromSuccess $ runParser parseExpr "11 && 0 && 1") @?= Just 0
    evalExpr Map.empty (fromSuccess $ runParser parseExpr "10 - 2 * 5 || 31 - 30 - 1 || 22 - 11 * 2") @?= Just 0
    evalExpr Map.empty (fromSuccess $ runParser parseExpr "1 || 2 || 3 || 4") @?= Just 1
    evalExpr Map.empty (fromSuccess $ runParser parseExpr "! 5") @?= Just 0
    evalExpr Map.empty (fromSuccess $ runParser parseExpr "! 0") @?= Just 1
    let sub = Map.fromList [("x", 15), ("y", 0), ("z", 32)]
    evalExpr sub (fromSuccess $ runParser parseExpr "x") @?= Just 15
    evalExpr sub (fromSuccess $ runParser parseExpr "-z") @?= Just (-32)
    evalExpr sub (fromSuccess $ runParser parseExpr "x * y + z - 2") @?= Just 30
    -- Failure:
    evalExpr sub (fromSuccess $ runParser parseExpr "x * y + abc") @?= Nothing

