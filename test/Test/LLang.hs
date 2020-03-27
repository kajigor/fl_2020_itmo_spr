module Test.LLang where

import Data.List (intercalate)
import AST              (AST (..), Operator (..))
import Combinators      (Result (..), runParser)
import LLang
import Test.Tasty.HUnit (Assertion, (@?=), assertBool)

isFailure (Failure _) = True
isFailure  _          = False

codePiece1 = intercalate "\n"
  ["read x;",
   "if (x >= 5)",
   "then {",
   "     x = x * 5;",
   "     y = 15;",
   "} else {",
   "     x = x ^ 5;",
   "     y = 22;",
   "}",
   "write (x);",
   "write(y);"]

statement1 =
  Seq [
    Read "x",
    If (BinOp Ge (Ident "x") (Num 5))
       -- Then
       (Seq [
          Assign "x" (BinOp Mult (Ident "x") (Num 5)),
          Assign "y" (Num 15)
        ])
       -- Else
       (Seq [
          Assign "x" (BinOp Pow (Ident "x") (Num 5)),
          Assign "y" (Num 22)
        ]),
    Write (Ident "x"),
    Write (Ident "y")
  ]

codePiece2 = intercalate "\n"
  ["if (x ==",
   "25 * 3 +",
   "4)",
   "then {",
   "    read x;}",
   "else {",
   "}"]

statement2 =
  If (BinOp Equal (Ident "x") (BinOp Plus (BinOp Mult (Num 25) (Num 3)) (Num 4)))
     (Read "x")
     (Seq [])

unit_parseL :: Assertion
unit_parseL = do
  runParser parseL " x = -15; " @?= Success "" (Assign "x" (UnaryOp Minus $ Num 15))
  runParser parseL "read hello;" @?= Success "" (Read "hello")
  runParser parseL "write (hello * 12 - 5);" @?= Success ""
    (Write (BinOp Minus (BinOp Mult (Ident "hello") (Num 12)) (Num 5)))
  runParser parseL " if(x > - 5 ) then { y = 12 ^ 40;} else {read x;}" @?= Success "" 
    (If (BinOp Gt (Ident "x") (UnaryOp Minus (Num 5)))
        (Assign "y" (BinOp Pow (Num 12) (Num 40)))
        (Read "x"))
  runParser parseL " while (varName /= 12) {  read varName; }" @?= Success ""
    (While (BinOp Nequal (Ident "varName") (Num 12))
           (Read "varName"))
  runParser parseL codePiece1 @?= Success "" statement1
  runParser parseL codePiece2 @?= Success "" statement2

unit_parseLEmptyBlocks :: Assertion
unit_parseLEmptyBlocks = do
  runParser parseL "if (x) then {} else {}" @?= Success ""
    (If (Ident "x")
        (Seq [])
        (Seq []))
  runParser parseL "if (x) then {x = 5;} else {  }" @?= Success ""
    (If (Ident "x")
        (Assign "x" (Num 5))
        (Seq []))
  runParser parseL "while (y) {}" @?= Success ""
    (While (Ident "y") (Seq []))
  runParser parseL "while (x) {while(y) {}}" @?= Success ""
    (While (Ident "x")
           (While (Ident "y") (Seq [])))

unit_parseLFailures :: Assertion
unit_parseLFailures = do
  -- Missing semicolon
  assertBool "" $ isFailure $ runParser parseL " x = -15"
  assertBool "" $ isFailure $ runParser parseL "read x x = 5;"
  assertBool "" $ isFailure $ runParser parseL "if(x > 5) then {y=15} else {x=20}"
  -- Missing curly bracket
  assertBool "" $ isFailure $ runParser parseL "if(x > 5) then {y=15;} else {read x;"
  assertBool "" $ isFailure $ runParser parseL "while (x < 5) x = x + 1;}"
  -- Missing round brackets
  assertBool "" $ isFailure $ runParser parseL "write x"
  assertBool "" $ isFailure $ runParser parseL "if x > 5 then {read x;} else {write x;}"
  assertBool "" $ isFailure $ runParser parseL "while x {x = 15}"
  -- Recursive fail
  assertBool "" $ isFailure $ runParser parseL "while (x < 5) {while (y > 5) {x = 12}}"
  assertBool "" $ isFailure $ runParser parseL "if (x == 5) then { while (y < 12) { y = y + 1;}} else { x = 5}"

unit_parseLAssignToKeyword :: Assertion
unit_parseLAssignToKeyword = do
  assertBool "" $ isFailure $ runParser parseL "write = 15;"
  runParser parseL "writeToLog = 20;" @?= Success "" (Assign "writeToLog" (Num 20))
  assertBool "" $ isFailure $ runParser parseL "if (x > 5) then {x = 5;} else {read=15;}"
  runParser parseL "readCount = 15 + 5;" @?= Success "" (Assign "readCount" (BinOp Plus (Num 15) (Num 5)))
  assertBool "" $ isFailure $ runParser parseL "if = 12;"
  assertBool "" $ isFailure $ runParser parseL "then = 10;"
  assertBool "" $ isFailure $ runParser parseL "else = 15 * 2;"
  runParser parseL "ifcase = 10; thendo = 15; elseStay = 13;" @?= Success ""
    (Seq [(Assign "ifcase" $ Num 10),
          (Assign "thendo" $ Num 15),
          (Assign "elseStay" $ Num 13)])
  assertBool "" $ isFailure $ runParser parseL "while (x > 5) {while = 13;}"
  runParser parseL "whileCase = 13;" @?= Success "" (Assign "whileCase" $ Num 13)


unit_parseLPartialParse :: Assertion
unit_parseLPartialParse = do
  runParser parseL " x = 20; y = 5 " @?= Success "y = 5 " (Assign "x" (Num 20))
  runParser parseL " if (x) then {read x;} else {write (x);} read x" @?= Success "read x"
    (If (Ident "x")
        (Read "x")
        (Write (Ident "x")))
  runParser parseL " read x; while x {}" @?= Success "while x {}" (Read "x")
