module Test.Optimizer where

import           Optimizer
import           UberExpr                       ( uberExpr )
import           Test.Helper
import           Test.Tasty.HUnit               ( Assertion
                                                , assertBool
                                                , (@?=)
                                                )

import           AST                            ( AST(..)
                                                , Operator(..)
                                                )

unit_optimizeNoIdent :: Assertion
unit_optimizeNoIdent = do
    evaluateOptimized "1" @?= Just (Num 1)
    evaluateOptimized "1+2" @?= Just (Num (1 + 2))
    evaluateOptimized "2+4+8" @?= Just (Num (2 + 4 + 8))
    evaluateOptimized "11+22" @?= Just (Num (11 + 22))
    evaluateOptimized "13+42+777" @?= Just (Num (13 + 42 + 777))
    evaluateOptimized "31+24+777" @?= Just (Num (31 + 24 + 777))
    evaluateOptimized "1+2*3+4" @?= Just (Num (1 + 2 * 3 + 4))
    evaluateOptimized "12+23*34+456" @?= Just (Num (12 + 23 * 34 + 456))
    evaluateOptimized "(1+2)*(3+4)" @?= Just (Num ((1 + 2) * (3 + 4)))
    evaluateOptimized "12+(23*(34)+456)" @?= Just (Num (12 + (23 * (34) + 456)))

unit_optimizeWithIdentifiers :: Assertion
unit_optimizeWithIdentifiers = do
    evaluateOptimized "x*x" @?= Just (BinOp Mult (Ident "x") (Ident "x"))
    evaluateOptimized "y*x*3"
        @?= Just (BinOp Mult (BinOp Mult (Ident "y") (Ident "x")) (Num 3))
    evaluateOptimized "abc" @?= Just (Ident "abc")

    evaluateOptimized "1*x*3" @?= Just (BinOp Mult (Ident "x") (Num 3))

    evaluateOptimized "2*5+x*0+y" @?= Just
        (BinOp Plus
               (Num 10)
               (Ident "y")
        )

    evaluateOptimized "1+y*3+z" @?= Just
        (BinOp Plus
               (BinOp Plus (Num 1) (BinOp Mult (Ident "y") (Num 3)))
               (Ident "z")
        )

    evaluateOptimized "y*x*3+11*x"
        @?= Just (BinOp Plus (BinOp Mult (BinOp Mult (Ident "y") (Ident "x")) (Num 3)) (BinOp Mult (Num 11) (Ident "x")))

    evaluateOptimized "y*x*3+11*5"
        @?= Just (BinOp Plus (BinOp Mult (BinOp Mult (Ident "y") (Ident "x")) (Num 3)) (Num 55))

    evaluateOptimized "11*0+y*x*3"
        @?= Just (BinOp Mult (BinOp Mult (Ident "y") (Ident "x")) (Num 3)) 

    evaluateOptimized "y*x*3+11*0"
        @?= Just (BinOp Mult (BinOp Mult (Ident "y") (Ident "x")) (Num 3)) 

