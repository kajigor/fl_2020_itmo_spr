module UberExpr where

import           Combinators (Parser (..), alt)

data Associativity = LeftAssoc | RightAssoc | NoAssoc

data OpType = Binary Associativity
            | Unary

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast

uberExpr [] termP _  _ = termP

uberExpr ((op, Binary LeftAssoc):opParsers) termP f g = do
  lst <- sepByOp op (uberExpr opParsers termP f g)
  return $ snd (foldl1' f lst)

uberExpr ((op, Binary RightAssoc):opParsers) termP f g = do
  lst <- sepByOp op (uberExpr opParsers termP f g)
  return $ snd (foldr1' f lst)

uberExpr ((op, Binary NoAssoc):opParsers) termP f g = parser `alt` uberExpr opParsers termP f g
   where parser = do
            term <- uberExpr opParsers termP f g 
            operator <- op
            term2 <- uberExpr opParsers termP f g 
            return $ f operator term term2 

uberExpr ((op, Unary):opParsers) termP f g = parser `alt` uberExpr opParsers termP f g
    where parser = do
            operator <- op
            term <- uberExpr opParsers termP f g 
            return $ g operator term

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
 
