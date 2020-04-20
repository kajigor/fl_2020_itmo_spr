module Test.LLang where

import Data.List (intercalate)
import AST              (AST (..), Operator (..))
import qualified Data.Map as Map
import Combinators      (Result (..), Position (..), toStream, runParser)
import LLang            (Configuration (..), LAst (..), parseL, Program(..), Function(..), parseDef, parseProg,
                         eval, initialConf)
import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Test.Helper

codePiece1 = intercalate "\n"
  ["read (x);",
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
   "    read (x);}",
   "else {",
   "}"]

statement2 =
  If (BinOp Equal (Ident "x") (BinOp Plus (BinOp Mult (Num 25) (Num 3)) (Num 4)))
     (Read "x")
     (Seq [])

programCode1 = intercalate "\n"
 ["def foo(x) {",
  "    return (x);",
  "}",
  "",
  "",
  "def main() {",
  "    return (foo(x));",
  "}"]

program1 = Program
  [Function "foo" ["x"] (Return $ Ident "x") ]
  (Return $ FunctionCall "foo" [Ident "x"])

unit_parseProg :: Assertion
unit_parseProg = do
  testSuccessErase (runParser parseProg programCode1) "" program1

unit_parseL :: Assertion
unit_parseL = do
  testSuccess (runParser parseL " x = -15; ") (toStream "" $ Position 1 10) (Assign "x" (UnaryOp Minus $ Num 15))
{-  
  runParser parseL "read hello;" @?= Success "" (Read "hello")
  runParser parseL "write (hello * 12 - 5);" @?= Success ""
    (Write (BinOp Minus (BinOp Mult (Ident "hello") (Num 12)) (Num 5)))
  runParser parseL " if(x > - 5 ) then { y = 12 ^ 40;} else {read x;}" @?= Success "" 
    (If (BinOp Gt (Ident "x") (UnaryOp Minus (Num 5)))
        (Assign "y" (BinOp Pow (Num 12) (Num 40)))
        (Read "x"))
-}
  testSuccessErase (runParser parseL " while (varName /= 12) {  read (varName); }") ""
    (While (BinOp Nequal (Ident "varName") (Num 12))
           (Read "varName"))
  testSuccess (runParser parseL codePiece1) (toStream "" $ Position 11 9) statement1
  testSuccess (runParser parseL codePiece2) (toStream "" $ Position 7 1) statement2

unit_parseLEmptyBlocks :: Assertion
unit_parseLEmptyBlocks = do
  testSuccess (runParser parseL "if (x) then {} else {}") (toStream "" $ Position 1 22)
    (If (Ident "x")
        (Seq [])
        (Seq []))
{-
  runParser parseL "if (x) then {x = 5;} else {  }" @?= Success ""
    (If (Ident "x")
        (Assign "x" (Num 5))
        (Seq []))
  runParser parseL "while (y) {}" @?= Success ""
    (While (Ident "y") (Seq []))
  runParser parseL "while (x) {while(y) {}}" @?= Success ""
    (While (Ident "x")
           (While (Ident "y") (Seq [])))
-}

unit_parseLFailures :: Assertion
unit_parseLFailures = do
  -- Missing semicolon
  testFailure $ runParser parseL " x = -15"
  testFailure $ runParser parseL "read (x) x = 5;"
  testFailure $ runParser parseL "if(x > 5) then {y=15} else {x=20}"
  -- Missing curly bracket
  testFailure $ runParser parseL "if(x > 5) then {y=15;} else {read x;"
  testFailure $ runParser parseL "while (x < 5) x = x + 1;}"
  -- Missing round brackets
  testFailure $ runParser parseL "write x"
  testFailure $ runParser parseL "if x > 5 then {read x;} else {write x;}"
  testFailure $ runParser parseL "while x {x = 15}"
  testFailure $ runParser parseL "read x;"
  -- Recursive fail
  testFailure $ runParser parseL "while (x < 5) {while (y > 5) {x = 12}}"
  testFailure $ runParser parseL "if (x == 5) then { while (y < 12) { y = y + 1;}} else { x = 5}"

unit_parseLAssignToKeyword :: Assertion
unit_parseLAssignToKeyword = do
  testFailure $ runParser parseL "write = 15;"
{-
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
-}

unit_parseLPartialParse :: Assertion
unit_parseLPartialParse = do
  testSuccessErase (runParser parseL " x = 20; y = 5 ") "y = 5 " (Assign "x" (Num 20))
{-
  runParser parseL " if (x) then {read x;} else {write (x);} read x" @?= Success "read x"
    (If (Ident "x")
        (Read "x")
        (Write (Ident "x")))
  runParser parseL " read x; while x {}" @?= Success "while x {}" (Read "x")
-}

-- read x;
-- if (x > 13)
-- then { write x }
-- else {
--     while (x < 42) {
--       x := x * 7;
--       write (x);
--     }
-- }
stmt1 :: LAst
stmt1 =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("X", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])


-- read x;
-- if (x)
-- then {
--   while (x) {
--     x := x - 2;
--     write (x);
--   }
-- else {}
stmt2 :: LAst
stmt2 =
  Seq
    [ Read "x"
    , If (Ident "x")
         (While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))

-- read x;
-- read y;
-- write (x == y);
stmt3 :: LAst
stmt3 =
  Seq
    [ Read "x"
    , Read "y"
    , Write (BinOp Equal (Ident "x") ((Ident "y")))
    ]

unit_stmt3 :: Assertion
unit_stmt3 = do
  let subst x y = Map.fromList [("x", x), ("y", y) ]
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
  eval stmt3 (initialConf [42]) @?= Nothing

-- read n;
-- if (n == 1 || n == 2)
-- then {
--   write 1;
-- }
-- else {
--   i := 2;
--   cur := 1
--   prev := 1
--   while (i < n) {
--     temp := cur + prev;
--     prev := cur;
--     cur := temp;
--     i := i + 1;
--   }
--   write (cur);
-- }
stmt4 :: LAst
stmt4 =
  Seq
    [ Read "n"
    , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
         (Write (Num 1))
         (Seq
            [ Assign "i" (Num 2)
            , Assign "cur" (Num 1)
            , Assign "prev" (Num 1)
            , While (BinOp Lt (Ident "i") (Ident "n"))
                     (Seq
                        [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
                        , Assign "prev" (Ident "cur")
                        , Assign "cur" (Ident "temp")
                        , Assign "i" (BinOp Plus (Ident "i") (Num 1))
                        ]
                     )
            , Write (Ident "cur")
            ]
         )
    ]

unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing

