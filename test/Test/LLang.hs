module Test.LLang where

import           AST
import           Combinators
import           Expr
import           LLang
import           Test.Tasty.HUnit

isFailure (Failure _) = True
isFailure  _          = False

unit_unaryEpxr = do
  runParser ident "asd" @?= Success "" "asd"

  runParser parseL "(read asd)" @?= Success ""
    (
      Seq [ Read "asd"
          ]
    )

  runParser parseL "(read asd) \
                   \(write asd)" @?= Success ""
    (
      Seq [ Read "asd"
          , Write (Ident "asd")
          ]
    )

  runParser parseL "(read n) \
                   \(read d) \
                   \(while ( n>0 ) { \
                   \      (n := n-d) \
                   \    })" @?= Success ""
    (
      Seq [ Read "n"
          , Read "d"
          , While (BinOp Gt (Ident "n") (Num 0)) $ Seq [
              Assign "n" (BinOp Minus (Ident "n") (Ident "d"))
              ]
          ]
    )


  runParser parseL "(read n) \
                   \(read d) \
                   \(while ( n>0 ) { \
                   \      (n := n-d) \
                   \      (write n) \
                   \    })" @?= Success ""
    (
      Seq [ Read "n"
          , Read "d"
          , While (BinOp Gt (Ident "n") (Num 0)) $ Seq
            [ Assign "n" (BinOp Minus (Ident "n") (Ident "d"))
            , Write (Ident "n")
            ]
          ]
    )
