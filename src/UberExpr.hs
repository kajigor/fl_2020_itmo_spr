module UberExpr where

import  Combinators
import Control.Applicative (Alternative (..))  
data Associativity = LeftAssoc | RightAssoc | NoAssoc

uberExpr :: Monoid e
         => [(Parser e i op, Associativity)]
         -> Parser e i ast
         -> (op -> ast -> ast -> ast)
         -> Parser e i ast
uberExpr [] prim f = prim
uberExpr ((p, LeftAssoc):ps) prim f = do
    (n, ops) <- sepBy1L p (uberExpr ps prim f)
    return $ foldl (\acc (op, term) -> f op acc term) n ops

uberExpr ((p,RightAssoc):ps) prim f = do
     (ops, n) <- sepBy1R p (uberExpr ps prim f)
     return $ foldr (\(term, op) acc -> f op term acc) n ops

uberExpr ((p,NoAssoc):ps) prim f = (do
    l <- prim
    op <- p
    r <- prim
    return $ f op l r
    ) <|> prim