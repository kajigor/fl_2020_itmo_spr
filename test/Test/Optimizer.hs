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

unit_optimize :: Assertion
unit_optimize = do
    evaluateOptimized "1" @?= Just (Num 1)
--     evaluateOptimized "1+2"               @?= Just $ Num (1 + 2)
--     evaluateOptimized "2+4+8"             @?= Just $ Num (2 + 4 + 8)
--     evaluateOptimized "11+22"             @?= Just $ Num (11 + 22)
--     evaluateOptimized "13+42+777"         @?= Just $ Num (13 + 42 + 777)
--     evaluateOptimized "31+24+777"         @?= Just $ Num (31 + 24 + 777)
--     evaluateOptimized "1+2*3+4"           @?= Just $ Num (1 + 2 * 3 + 4)
--     evaluateOptimized "12+23*34+456"      @?= Just $ Num (12 + 23 * 34 + 456)
--     evaluateOptimized "1-2*3+4"           @?= Just $ Num (1 - 2 * 3 + 4)
--     evaluateOptimized "1-2-3"             @?= Just $ Num (1 - 2 - 3)
--     evaluateOptimized "4/2-2"             @?= Just $ Num (4 `div` 2 - 2)
--     evaluateOptimized "(1+2)*(3+4)"       @?= Just $ Num ((1 + 2) * (3 + 4))
--     evaluateOptimized "12+(23*(34)+456)"  @?= Just $ Num (12 + (23 * (34) + 456))
--     evaluateOptimized "((1-(2*3))+4)"     @?= Just $ Num ((1 - (2 * 3)) + 4)
--     evaluateOptimized "1-2+3-4"           @?= Just $ Num (1 - 2 + 3 - 4)
--     evaluateOptimized "6/2*3"             @?= Just $ Num (6 `div` 2 * 3)
-- 
-- unit_optimizeWithIdentifiers :: Assertion
-- unit_optimizeWithIdentifiers = do
--     evaluateOptimized "x*x" @?= Just (BinOp Mult (Ident "x") (Ident "x"))
--     evaluateOptimized "y*x*3"
--         @?= Just (BinOp Mult (BinOp Mult (Ident "y") (Ident "x")) (Num 3))
--     evaluateOptimized "abc" @?= Just (Ident "abc")
-- 
--     evaluateOptimized "1*x*3" @?= Just (BinOp Mult (Ident "x") (Num 3))
--     evaluateOptimized "2*5+x*0+y" @?= Just
--         (BinOp Plus
--                (BinOp Mult (Num 1) (Ident "x"))
--                (BinOp Mult (Ident "z") (Num 4))
--         )
-- 
--     evaluateOptimized "1+y*3+z" @?= Just
--         (BinOp Plus
--                (BinOp Plus (Num 1) (BinOp Mult (Ident "y") (Num 3)))
--                (Ident "z")
--         )
-- 
-- 
