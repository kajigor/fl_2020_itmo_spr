module UberExpr where

import Combinators
import Control.Applicative (Alternative (..))  
data Associativity = LeftAssoc | RightAssoc | NoAssoc

data OpType = Binary Associativity
            | Unary

parse p = do
    x <- p
    return $ x:[]
    -- xs <- parse p <|> return []
    -- return (x:xs)

uberExpr :: (Monoid e, Eq e)
         => [(Parser e i op, OpType)]
         -> Parser e i ast
         -> (op -> ast -> ast -> ast)
         ->  (op -> ast -> ast)
         -> Parser e i ast
uberExpr [] prim f g = prim

uberExpr ((p, Unary):ps) prim f g = (do
    op <- p 
    term <- uberExpr ps prim f g
    return $ g op term) <|> uberExpr ps prim f g


-- uberExpr ((p, Unary):ps) prim f g = (do
--     (op:ops) <- parse p
--     term <- uberExpr ps prim f g
--     let result = foldl (\acc op -> g op acc) (g op term) ops
--     return $ result) <|> uberExpr ps prim f g

uberExpr ((p, Binary LeftAssoc):ps) prim f g = do
    (n, ops) <- sepBy1L p (uberExpr ps prim f g)
    return $ foldl (\acc (op, term) -> f op acc term) n ops

uberExpr ((p, Binary RightAssoc):ps) prim f g = do
     (ops, n) <- sepBy1R p (uberExpr ps prim f g)
     return $ foldr (\(term, op) acc -> f op term acc) n ops

uberExpr ((p, Binary NoAssoc):ps) prim f g =
  (do l <- uberExpr ps prim f g
      op <- p
      r <- uberExpr ps prim f g
      return $ f op l r) <|>
  uberExpr ps prim f g