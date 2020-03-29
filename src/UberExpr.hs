module UberExpr where

import           Combinators (Parser (..), alt)

data Associativity = LeftAssoc | RightAssoc | NoAssoc

uberExpr :: Monoid e
         => [(Parser e i op, Associativity)]
         -> Parser e i ast
         -> (op -> ast -> ast -> ast)
         -> Parser e i ast

uberExpr [] termP _ = termP

uberExpr ((op, LeftAssoc):opParsers) termP f = do
  lst <- sepByOp op (uberExpr opParsers termP f)
  return $ snd (foldl1' f lst)

uberExpr ((op, RightAssoc):opParsers) termP f = do
  lst <- sepByOp op (uberExpr opParsers termP f)
  return $ snd (foldr1' f lst)

uberExpr ((op, NoAssoc):opParsers) termP f = parser `alt` termP
   where parser = do
          term <- termP
          operator <- op
          term2 <- uberExpr opParsers termP f
          return $ f operator term term2

foldl1' :: (op -> ast -> ast -> ast) -> [(Maybe op, ast)] -> (Maybe op, ast)
foldl1' f [x] = x
foldl1' f (a:(b:xs)) = foldl1' f ((op', f op x x'):xs) 
        where (Just op, x) = a
              (op', x') = b

foldr1' :: (op -> ast -> ast -> ast) -> [(Maybe op, ast)] -> (Maybe op, ast)
foldr1' f [x] = x
foldr1' f ((Just op, x):xs) = (op', f op x x')
        where (op', x') = foldr1' f xs

sepByOp :: Monoid e => Parser e i op -> Parser e i ast -> Parser e i [(Maybe op, ast)]
sepByOp opP termP = parser `alt` parser2
        where parser = do
                term <- termP
                op <- opP
                terms <- sepByOp opP termP        
                return $ (Just op, term):terms
              parser2 = do
                term <- termP
                return [(Nothing, term)]
 
