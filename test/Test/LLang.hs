{-# LANGUAGE QuasiQuotes #-}

module Test.LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Result (..), runParser, separator, space,
                                      string)
import           Control.Applicative (many)
import qualified Data.Map            as Map
import           LLang
import           Test.Tasty.HUnit    (Assertion, assertBool, (@?=))
import           Text.RawString.QQ

isFailure (Failure _) = True
isFailure _           = False

unit_TestAssign :: Assertion
ass = show <$> expression'

unit_TestAssign = do
  runParser parseL [r| let name = 4;|] @?= Success "" (Seq {statements = [Assign {var = "name", expr = Num 4}]})
  runParser parseL [r|let    number    =     42 + 34234 * 3 + idt ;|] @?=
    Success
      ""
      (Seq
         { statements =
             [ Assign
                 { var = "number"
                 , expr = BinOp Plus (BinOp Plus (Num 42) (BinOp Mult (Num 34234) (Num 3))) (Ident "idt")
                 }
             ]
         })
  runParser parseL [r|leta = 4;|] @?= Success "leta = 4;" (Seq [])
  runParser parseL [r|let write = 4;|] @?= Success "let write = 4;" (Seq [])
  runParser parseL [r|let write = let;|] @?= Success "let write = let;" (Seq [])

unit_TestIf :: Assertion
unit_TestIf = do
  runParser
    parseL
    [r|
    if a == 4 {
    } else {
    }|] @?=
    Success
      ""
      (Seq
         { statements =
             [If {cond = BinOp Equal (Ident "a") (Num 4), thn = Seq {statements = []}, els = Seq {statements = []}}]
         })
  runParser parseL [r|if a == 4{}else{}|] @?=
    Success
      ""
      (Seq
         { statements =
             [If {cond = BinOp Equal (Ident "a") (Num 4), thn = Seq {statements = []}, els = Seq {statements = []}}]
         })
  runParser parseL [r|  if a >= 4 && name <= 43 { let a = 4332; } else {let a = 4332;}|] @?=
    Success
      ""
      (Seq
         { statements =
             [ If
                 { cond = BinOp And (BinOp Ge (Ident "a") (Num 4)) (BinOp Le (Ident "name") (Num 43))
                 , thn = Seq {statements = [Assign {var = "a", expr = Num 4332}]}
                 , els = Seq {statements = [Assign {var = "a", expr = Num 4332}]}
                 }
             ]
         })
  runParser parseL [r| if if + 4 then {} else {}|] @?= Success " if if + 4 then {} else {}" (Seq [])

unit_TestWhile :: Assertion
unit_TestWhile = do
  runParser
    parseL
    [r|
    while a == 4 {
    }|] @?=
    Success "" (Seq {statements = [While {cond = BinOp Equal (Ident "a") (Num 4), body = Seq {statements = []}}]})
  runParser parseL [r|while a == 4{}|] @?=
    Success "" (Seq {statements = [While {cond = BinOp Equal (Ident "a") (Num 4), body = Seq {statements = []}}]})
  runParser
    parseL
    [r| while a >= 4 && name <= 43 {
          if name + 423 * 4 == a {
            let a = 5435344;
          } else {
            let name = 435 + 4453;
          }
        }|] @?=
    Success
      ""
      (Seq
         { statements =
             [ While
                 { cond = BinOp And (BinOp Ge (Ident "a") (Num 4)) (BinOp Le (Ident "name") (Num 43))
                 , body =
                     Seq
                       { statements =
                           [ If
                               { cond =
                                   BinOp Equal (BinOp Plus (Ident "name") (BinOp Mult (Num 423) (Num 4))) (Ident "a")
                               , thn = Seq {statements = [Assign {var = "a", expr = Num 5435344}]}
                               , els =
                                   Seq {statements = [Assign {var = "name", expr = BinOp Plus (Num 435) (Num 4453)}]}
                               }
                           ]
                       }
                 }
             ]
         })
  runParser parseL [r| while if + 4 then {} else {}|] @?= Success " while if + 4 then {} else {}" (Seq [])

unit_TestRead = do
  runParser parseL [r| read _name   ;   |] @?= Success "" (Seq {statements = [Read {var = "_name"}]})
  runParser parseL [r| read if   ;  |] @?= Success " read if   ;  " (Seq {statements = []})
  runParser parseL [r| read name + 234   ;  |] @?= Success " read name + 234   ;  " (Seq {statements = []})
  runParser parseL [r| read (a + b)   ;  |] @?= Success " read (a + b)   ;  " (Seq {statements = []})

unit_TestWrite = do
  runParser parseL [r| write _name   ;   |] @?= Success "" (Seq {statements = [Write {expr = Ident "_name"}]})
  runParser parseL [r| write name + 234   ;  |] @?=
    Success "" (Seq {statements = [Write {expr = BinOp Plus (Ident "name") (Num 234)}]})
  runParser parseL [r| write (a + b)   ;  |] @?=
    Success "" (Seq {statements = [Write {expr = BinOp Plus (Ident "a") (Ident "b")}]})

unit_TestSeq = do
  runParser
    parseL
    [r|
      let a = 4 == 6;
      let b = 3;
      while a && b > 5 {
        if (4 > 5) && (name < 234) {
          if 4 == 5 {
            let c = 44;
            write (c + b) * c;
          } else {
            read c;
          }
        }else {
          read x;
        }
      }
    |] @?=
    Success
      ""
      (Seq
         { statements =
             [ Assign {var = "a", expr = BinOp Equal (Num 4) (Num 6)}
             , Assign {var = "b", expr = Num 3}
             , While
                 { cond = BinOp And (Ident "a") (BinOp Gt (Ident "b") (Num 5))
                 , body =
                     Seq
                       { statements =
                           [ If
                               { cond = BinOp And (BinOp Gt (Num 4) (Num 5)) (BinOp Lt (Ident "name") (Num 234))
                               , thn =
                                   Seq
                                     { statements =
                                         [ If
                                             { cond = BinOp Equal (Num 4) (Num 5)
                                             , thn =
                                                 Seq
                                                   { statements =
                                                       [ Assign {var = "c", expr = Num 44}
                                                       , Write
                                                           { expr =
                                                               BinOp
                                                                 Mult
                                                                 (BinOp Plus (Ident "c") (Ident "b"))
                                                                 (Ident "c")
                                                           }
                                                       ]
                                                   }
                                             , els = Seq {statements = [Read {var = "c"}]}
                                             }
                                         ]
                                     }
                               , els = Seq {statements = [Read {var = "x"}]}
                               }
                           ]
                       }
                 }
             ]
         })
  runParser
    parseL
    [r|if 4==5 {read _name_;write _name_;read _name_;write _name_;} else {read _name_;write _name_;read _name_;write _name_;}|] @?=
    Success
      ""
      (Seq
         { statements =
             [ If
                 { cond = BinOp Equal (Num 4) (Num 5)
                 , thn =
                     Seq
                       { statements =
                           [ Read {var = "_name_"}
                           , Write {expr = Ident "_name_"}
                           , Read {var = "_name_"}
                           , Write {expr = Ident "_name_"}
                           ]
                       }
                 , els =
                     Seq
                       { statements =
                           [ Read {var = "_name_"}
                           , Write {expr = Ident "_name_"}
                           , Read {var = "_name_"}
                           , Write {expr = Ident "_name_"}
                           ]
                       }
                 }
             ]
         })

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
    , If
        (BinOp Gt (Ident "X") (Num 13))
        (Write (Ident "X"))
        (While (BinOp Lt (Ident "X") (Num 42)) (Seq [Assign "X" (BinOp Mult (Ident "X") (Num 7)), Write (Ident "X")]))
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
    , If
        (Ident "x")
        (While (Ident "x") (Seq [(Assign "x" (BinOp Minus (Ident "x") (Num 2))), (Write (Ident "x"))]))
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
stmt3 = Seq [Read "x", Read "y", Write (BinOp Equal (Ident "x") ((Ident "y")))]

unit_stmt3 :: Assertion
unit_stmt3 = do
  let subst x y = Map.fromList [("x", x), ("y", y)]
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
    , If
        (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
        (Write (Num 1))
        (Seq
           [ Assign "i" (Num 2)
           , Assign "cur" (Num 1)
           , Assign "prev" (Num 1)
           , While
               (BinOp Lt (Ident "i") (Ident "n"))
               (Seq
                  [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
                  , Assign "prev" (Ident "cur")
                  , Assign "cur" (Ident "temp")
                  , Assign "i" (BinOp Plus (Ident "i") (Num 1))
                  ])
           , Write (Ident "cur")
           ])
    ]

unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55])
  eval stmt4 (initialConf []) @?= Nothing
