{-# LANGUAGE QuasiQuotes #-}

module Test.LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Result (..), runParser, separator, space,
                                      string)
import           Control.Applicative (many)
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
