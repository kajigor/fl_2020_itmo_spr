module Test.LLang where

import           AST
import           Combinators      (Result (..), runParser, toStream)
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang            (Configuration (..), LAst (..), eval,
                                   initialConf, parseL, Function (..), Program (..))
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)
import           Data.List        (intercalate)

-- f x y = read z ; return (x + z * y)
-- g x = if (x) then return x else return x*13
-- {read x; read y; write (f x y); write (g x)}"

prog =
  Program
    [ Function "f" ["x", "y"] (Seq [Read "z", Return (BinOp Plus (Ident "x") (Ident "y"))])
    , Function "g" ["x"] (If (Ident "x") (Return (Ident "x")) (Return (BinOp Mult (Ident "x") (Num 13))))
    ]
    (
      Seq
        [ Read "x"
        , Read "y"
        , Write (FunctionCall "f" [Ident "x", Ident "y"])
        , Write (FunctionCall "g" [Ident "x"])
        ]
    )

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
    [ Read "x"
    , If (BinOp Gt (Ident "x") (Num 13))
         (Seq [(Write (Ident "x"))])
         (Seq [(While (BinOp Lt (Ident "x") (Num 42))
                (Seq [ Assign "x"
                        (BinOp Mult (Ident "x") (Num 7))
                     , Write (Ident "x")
                     ]
                )
         )])
    ]

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("x", n)]
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
         (Seq [(While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )])
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
         (Seq [(Write (Num 1))])
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
