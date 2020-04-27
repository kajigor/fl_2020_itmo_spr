module Test.LLang where

import           AST
import           Combinators      (Result (..), runParser, toStream)
import qualified Data.Map         as Map
import           Debug.Trace      (trace)
import           LLang            (Configuration (..), LAst (..), eval,
                                   initialConf, parseL, Function (..), Program (..), parseWord)

import           Test.Helper
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)

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

sub1 = "(while (x<42) ((let (x) (x*7))))"
sub1' = Seq [While (BinOp Lt (Ident "x") (Num 42)) (Seq [Assign "x" (BinOp Mult (Ident "x") (Num 7))])]

sub3 = "(while (x<42) ((let (x) (x*7)) (write (x))))"
sub3' = Seq [While (BinOp Lt (Ident "x") (Num 42)) (Seq [Assign "x" (BinOp Mult (Ident "x") (Num 7)), Write (Ident "x")])]

sub2 = "(if (x>13) then ((write (x))) else ((while (x<42) ((let (x) (x*7)) (write (x))))))"

sub2' = Seq [If (BinOp Gt (Ident "x") (Num 13))
         (Seq [(Write (Ident "x"))])
         (Seq [(While (BinOp Lt (Ident "x") (Num 42))
                (Seq [ Assign "x"
                        (BinOp Mult (Ident "x") (Num 7))
                     , Write (Ident "x")
                     ]
                )
         )])]

stmt1' :: String
stmt1' = "(read (x)) (if (x>13) then ((write (x))) else ((while (x<42) ((let (x) (x*7)) (write (x))))))"


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
--
stmt2' = "(read (x)) (if (x) then ((while (x) ((let (x) (x-2)) (write (x))))) else ())"

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
stmt3' = "(read (x)) (read (y)) (write (x==y))"
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

unit_parseWord :: Assertion
unit_parseWord = do
    let p = runParser parseWord
    testSuccess (p "var") (toStream "" 3) "var"
    testSuccess (p "var ") (toStream " " 3) "var"
    testSuccess (p "123wead...var ") (toStream " " (length "123wead...var")) "123wead...var"

unit_parseSeq :: Assertion
unit_parseSeq = do
    let p = runParser parseL
    let pp prog res = testSuccess (p prog) (toStream "" (length prog)) res
    let p1 = "(let (x) (12))"

    pp p1 (Seq [Assign "x" (Num 12)])
    pp (p1 ++ p1) (Seq [Assign "x" (Num 12),Assign "x" (Num 12)])

    let p2 = "(let (x) (12)) (write (y+1)) (read (z))"
    pp p2 (Seq [Assign "x" (Num 12),
                Write (BinOp Plus (Ident "y") (Num 1)),
                    Read "z"])
--
    let p3 = "(while (1) ((return (x))))"
    pp p3 (Seq [While (Num 1)
                    (Seq [Return (Ident "x")])])
--
    let p4 = "(if (1) then ((return (x))) else ((return (x))))"
    pp p4 (Seq [If (Num 1) (Seq [Return (Ident "x")]) (Seq [Return (Ident "x")])])
--
    let p5 = "(if (1) then ((let (x) (1))) else ((let (y) (12))))"
    pp p5 (Seq [If (Num 1) (Seq [Assign "x" (Num 1)]) (Seq [Assign "y" (Num 12)])])
--
    let p6 = "(if (1) then ((let (x) (1)) (write (x))) else ((let (y) (12))))"
    pp p6 (Seq [If (Num 1) (Seq [Assign "x" (Num 1), Write (Ident "x")]) (Seq [Assign "y" (Num 12)])])

    pp sub1 sub1'
    pp sub3 sub3'
    pp sub2 sub2'
    pp stmt1' stmt1
    pp stmt2' stmt2
    pp stmt3' stmt3

