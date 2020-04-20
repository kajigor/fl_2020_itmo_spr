{-# LANGUAGE QuasiQuotes #-}

module Test.LLang where

import           AST
import           Combinators       (InputStream (..), Position (..),
                                    Result (..), runParser, toStream, initPosition)
import qualified Data.Map          as Map
import           Debug.Trace       (trace, traceShowId)
import           LLang             (Configuration (..), Function (..),
                                    LAst (..), Program (..), eval, initialConf,
                                    parseL, parseProg)
import           Test.Helper
import           Test.Tasty.HUnit  (Assertion, assertBool, (@?=))
import           Text.Printf       (printf)
import           Text.RawString.QQ (r)

assertSuccess' str res ast = do
  let (Success (InputStream stream _) error result) = runParser parseL str
  result @?= ast
  stream @?= res

assertSuccess'' str res prog = do
  let (Success (InputStream stream _) error result) = runParser parseProg str
  result @?= prog
  stream @?= res

unit_TestAssign = do
  assertSuccess' [r|  name = 4;|] "" (Seq {statements = [Assign {var = "name", expr = Num 4}]})
  assertSuccess'
    [r|    number    =     42 + 34234 * 3 + idt ;|]
    ""
    (Seq
       { statements =
           [ Assign
               {var = "number", expr = BinOp Plus (BinOp Plus (Num 42) (BinOp Mult (Num 34234) (Num 3))) (Ident "idt")}
           ]
       })
  assertSuccess' [r| write = 4;|] " write = 4;" (Seq [])

unit_TestIf :: Assertion
unit_TestIf = do
  assertSuccess'
    [r|
    if a == 4 {
    } else {
    }|]
    ""
    (Seq
       { statements =
           [If {cond = BinOp Equal (Ident "a") (Num 4), thn = Seq {statements = []}, els = Seq {statements = []}}]
       })
  assertSuccess'
    [r|if a == 4{}else{}|]
    ""
    (Seq
       { statements =
           [If {cond = BinOp Equal (Ident "a") (Num 4), thn = Seq {statements = []}, els = Seq {statements = []}}]
       })
  assertSuccess'
    [r|  if a >= 4 && name <= 43 {  a = 4332; } else { a = 4332;}|]
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
  assertSuccess' [r| if if + 4 then {} else {}|] " if if + 4 then {} else {}" (Seq [])

unit_TestWhile :: Assertion
unit_TestWhile = do
  assertSuccess'
    [r|
    while a == 4 {
    }|]
    ""
    (Seq {statements = [While {cond = BinOp Equal (Ident "a") (Num 4), body = Seq {statements = []}}]})
  assertSuccess'
    [r|while a == 4{}|]
    ""
    (Seq {statements = [While {cond = BinOp Equal (Ident "a") (Num 4), body = Seq {statements = []}}]})
  assertSuccess'
    [r| while a >= 4 && name <= 43 {
          if name + 423 * 4 == a {
             a = 5435344;
          } else {
             name = 435 + 4453;
          }
        }|]
    ""
    (Seq
       { statements =
           [ While
               { cond = BinOp And (BinOp Ge (Ident "a") (Num 4)) (BinOp Le (Ident "name") (Num 43))
               , body =
                   Seq
                     { statements =
                         [ If
                             { cond = BinOp Equal (BinOp Plus (Ident "name") (BinOp Mult (Num 423) (Num 4))) (Ident "a")
                             , thn = Seq {statements = [Assign {var = "a", expr = Num 5435344}]}
                             , els = Seq {statements = [Assign {var = "name", expr = BinOp Plus (Num 435) (Num 4453)}]}
                             }
                         ]
                     }
               }
           ]
       })
  assertSuccess' [r| while if + 4 then {} else {}|] " while if + 4 then {} else {}" (Seq [])

unit_TestRead = do
  assertSuccess' [r| read _name   ;   |] "" (Seq {statements = [Read {var = "_name"}]})
  assertSuccess' [r| read if   ;  |] " read if   ;  " (Seq {statements = []})
  assertSuccess' [r| read name + 234   ;  |] " read name + 234   ;  " (Seq {statements = []})
  assertSuccess' [r| read (a + b)   ;  |] " read (a + b)   ;  " (Seq {statements = []})

unit_TestWrite = do
  assertSuccess' [r| write _name   ;   |] "" (Seq {statements = [Write {expr = Ident "_name"}]})
  assertSuccess'
    [r| write name + 234   ;  |]
    ""
    (Seq {statements = [Write {expr = BinOp Plus (Ident "name") (Num 234)}]})
  assertSuccess' [r| write (a + b)   ;  |] "" (Seq {statements = [Write {expr = BinOp Plus (Ident "a") (Ident "b")}]})

unit_TestSeq = do
  assertSuccess'
    [r|
       a = 4 == 6;
       b = 3;
      while a && b > 5 {
        if (4 > 5) && (name < 234) {
          if 4 == 5 {
             c = 44;
            write (c + b) * c;
          } else {
            read c;
          }
        }else {
          read x;
        }
      }
    |]
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
                                                             BinOp Mult (BinOp Plus (Ident "c") (Ident "b")) (Ident "c")
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
  assertSuccess'
    [r|if 4==5 {read _name_;write _name_;read _name_;write _name_;} else {read _name_;write _name_;read _name_;write _name_;}|]
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

-- f x y = read z ; return (x + y)
-- g x = if (x) then return x else return x*13
-- {read x; read y; write (f x y); write (g x)}"
prog =
  Program
    [ Function "f" ["x", "y"] (Seq [Read "z", Return (BinOp Plus (Ident "x") (Ident "y"))])
    , Function
        "g"
        ["x"]
        (Seq [If (Ident "x") (Seq [Return (Ident "x")]) (Seq [Return (BinOp Mult (Ident "x") (Num 13))])])
    ]
    (Seq [Read "x", Read "y", Write (FunctionCall "f" [Ident "x", Ident "y"]), Write (FunctionCall "g" [Ident "x"])])

unit_prog =
  assertSuccess''
    [r|
    func f(x,y) {
      read z;
      return (x + y);
    }
    func g(x) {
      if (x) {
        return x;
      } else {
        return x * 13;
      }
    }

    read x;
    read y;
    write f(x,y);
    write g(x);
  |]
    ""
    prog

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
    , If
        (BinOp Gt (Ident "x") (Num 13))
        (Seq [Write (Ident "x")])
        (Seq
           [ While
               (BinOp Lt (Ident "x") (Num 42))
               (Seq [Assign "x" (BinOp Mult (Ident "x") (Num 7)), Write (Ident "x")])
           ])
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
    , If
        (Ident "x")
        (Seq [While (Ident "x") (Seq [Assign "x" (BinOp Minus (Ident "x") (Num 2)), Write (Ident "x")])])
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
        (Seq [(Write (Num 1))])
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

unit_position :: Assertion
unit_position = do
  let prog = [r|a = 2 + 4 * 5;|]
  getPosition (runParser parseProg prog) @?= Just (Position 0 14)

  let prog = [r|while if + 4 {}|] -- `if` keyword in condition
  getPosition (runParser parseProg prog) @?= Just initPosition

  let prog = [r|
    while (a == 3) {
    }
    a = 3 + + 1;
  |]
  getPosition (runParser parseProg prog) @?= Just (Position 3 4) -- column 4 because of tab (4 spaces)
