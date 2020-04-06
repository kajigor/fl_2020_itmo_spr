module UberExpr where

import           Combinators (Parser (..), sepBy1', many')
import           Control.Applicative(Alternative (..))

data Associativity = LeftAssoc | RightAssoc | NoAssoc

data OpType = Binary Associativity
            | Unary

uberExpr :: Monoid e
         => [(Parser e i op, Associativity)]
         -> Parser e i ast
         -> (op -> ast -> ast -> ast)
         -> Parser e i ast

uberExpr [] elemParser f = elemParser

uberExpr ((parser, NoAssoc):ps) elemParser f = parse <|> uberExpr ps elemParser f
	where parse = do
		x <- uberExpr ps elemParser f
		s <- parser
		y <- uberExpr ps elemParser f
		return (f s x y)

uberExpr ((parser, LeftAssoc):ps) elemParser f = parse <|> elemParser
	where parse = do
		(x, rest) <- sepLeftHelper parser (uberExpr ps elemParser f)
		return (foldl (\x (y, z) -> f y x z) x rest)


uberExpr ((parser, RightAssoc):ps) elemParser f = parse <|> elemParser
	where parse = do
		(rest, x) <- sepRightHelper parser (uberExpr ps elemParser f)
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
