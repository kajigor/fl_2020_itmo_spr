module UberExpr where

import           Combinators (Parser (..), sepBy1', many')
import           Control.Applicative(Alternative (..))

data Associativity = LeftAssoc | RightAssoc | NoAssoc

data OpType = Binary Associativity
            | Unary

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast

uberExpr [] elemParser _ _ = elemParser

uberExpr ((parser, Unary):ps) elemParser f unary_f = parse <|> elemParser
	where parse = do
		s <- parser
		x <- uberExpr ps elemParser f unary_f
		return (unary_f s x)

uberExpr ((parser, Binary NoAssoc):ps) elemParser f unary_f  = parse <|> uberExpr ps elemParser f unary_f
	where parse = do
		x <- uberExpr ps elemParser f unary_f 
		s <- parser
		y <- uberExpr ps elemParser f unary_f 
		return (f s x y)

uberExpr ((parser, Binary LeftAssoc):ps) elemParser f unary_f = parse <|> elemParser
	where parse = do
		(x, rest) <- sepLeftHelper parser (uberExpr ps elemParser f unary_f)
		return (foldl (\x (y, z) -> f y x z) x rest)


uberExpr ((parser, Binary RightAssoc):ps) elemParser f unary_f = parse <|> elemParser
	where parse = do
		(rest, x) <- sepRightHelper parser (uberExpr ps elemParser f unary_f)
		return (foldr (\(x, y) z -> f y x z) x rest)


sepLeftHelper :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i (a, [(sep, a)])
sepLeftHelper = sepBy1' 

sepRightHelper :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i ([(a, sep)], a)
sepRightHelper sep elem = do
	let sepHelper sep elem = do
		e <- elem
		s <- sep
		return (e, s)
	xs <- many' (sepHelper sep elem)
	x <- elem
	return (xs, x)
