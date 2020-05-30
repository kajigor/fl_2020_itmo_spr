module Optimizer where


import           AST                            ( AST(..)
                                                , Operator(..)
                                                )
import           Control.Applicative
import           Expr                           ( parseExpr )
import           Combinators                    ( Parser(..)
                                                , Result(..)
                                                , InputStream(..)
                                                , runParser
                                                )

evaluateOptimized :: String -> Maybe AST
evaluateOptimized input = case runParser parseExpr input of
    Success rest _ ast | null (stream rest) -> optimize ast
    _ -> Nothing


optimize :: AST -> Maybe AST
optimize (BinOp Plus left right) = do
    left'  <- optimize left
    right' <- optimize right
    case (left', right') of
        (Num i, Num j) -> return $ Num (i + j)

        (Num i, BinOp Plus (Num j) r) -> return $ BinOp Plus (Num (i + j)) r
        (Num i, BinOp Plus l (Num j)) -> return $ BinOp Plus (Num (i + j)) l

        (BinOp Plus (Num j) r, Num i) -> return $ BinOp Plus (Num (i + j)) r
        (BinOp Plus l (Num j), Num i) -> return $ BinOp Plus (Num (i + j)) l

        _ -> return $ BinOp Plus left' right'

optimize (BinOp Mult left right) = do
    left'  <- optimize left
    right' <- optimize right
    case (left', right') of
        (Num i, Num j) -> return $ Num (i * j)

        (Num i, BinOp Mult (Num j) r) -> return $ BinOp Mult (Num (i * j)) r
        (Num i, BinOp Mult l (Num j)) -> return $ BinOp Mult (Num (i * j)) l

        (BinOp Mult (Num j) r, Num i) -> return $ BinOp Mult (Num (i * j)) r
        (BinOp Mult l (Num j), Num i) -> return $ BinOp Mult (Num (i * j)) l

        _ -> return $ BinOp Mult left' right'

optimize a@(Num _) = return a

optimize a = return a

-- optimize (  BinOp Mult left right) = do
--     left' <- optimize left
--     case left' of
--         Num i -> do
--             right' <- optimize right
--             case right' of
--                 Num j -> return $ Num (i * j)
--                 _     -> return $ BinOp Mult left' right'
--         _ -> optimize right >>= (\right' -> return $ BinOp Mult left' right')
-- 

{-     *
 -   /
    +
   / \
  +   N
 / \
N   N

-}
