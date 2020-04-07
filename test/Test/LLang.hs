module Test.LLang where

import           AST              
import           Combinators      
import           Expr             
                                   
import           Test.Tasty.HUnit (Assertion, (@?=))

import LLang



unit_LLang_Success :: Assertion
unit_LLang_Success = do
    runParser parseL "x=5;" @?= Success "" (Seq {statements = [Assign {var = "x", expr = Num 5}]})
    runParser parseL " x=5  ;" @?= Success "" (Seq {statements = [Assign {var = "x", expr = Num 5}]})
    runParser parseL " x =  5  ; " @?= Success "" (Seq {statements = [Assign {var = "x", expr = Num 5}]})
    runParser parseL " read(x_123_S);" @?= Success "" (Seq {statements = [Read {var = "x_123_S"}]})
    runParser parseL " read (  x_123_S  ) ; " @?= Success "" (Seq {statements = [Read {var =  "x_123_S"}]})
    runParser parseL " write(x_123_S);" @?= Success "" (Seq {statements = [Write {expr = Ident "x_123_S"}]})
    runParser parseL " write (  x_123_S  ) ; " @?= Success "" (Seq {statements = [Write {expr = Ident "x_123_S"}]})
    runParser parseL " write(10);" @?= Success "" (Seq {statements = [Write {expr = Num 10}]})
    runParser parseL " write (  5+3  ) ; " @?= Success "" (Seq {statements = [Write {expr = BinOp Plus (Num 5) (Num 3)}]})
    runParser parseL " while(x==5) {read(i);};" @?= Success "" (Seq {statements = [While {cond = BinOp Equal (Ident "x") (Num 5), body = Seq {statements = [Read {var = "i"}]}}]})

    runParser parseL " while  (  x==5  ) { write ( i ) ; };" @?= Success "" (Seq {statements = [While {cond = BinOp Equal (Ident "x") (Num 5), body = Seq {statements = [Write {expr = Ident "i"}]}}]})
    runParser parseL "if ( a&&b ) {read(i) ;} else {ss =  7;}; " @?= Success "" (Seq {statements = [If {cond = BinOp And (Ident "a") (Ident "b"), thn = Seq {statements = [Read {var = "i"}]}, els = Seq {statements = [Assign {var = "ss", expr = (Num 7)}]}}]})
    runParser parseL " x = 5  ; read(_w); write(0); " @?= Success "" (Seq {statements = [Assign {var = "x", expr = (Num 5)}, Read {var = "_w"}, Write {expr = (Num 0)}]})
