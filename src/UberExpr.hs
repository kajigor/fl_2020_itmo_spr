module UberExpr where

import           Combinators         (Parser (..), sepBy1', sepBy1'')
import           Control.Applicative (empty, (<|>))
import           Debug.Trace

data Associativity
  = LeftAssoc
  | RightAssoc
  | NoAssoc

data OpType
  = Binary Associativity
  | Unary

uberExpr ::
     Monoid e
  => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
  -> Parser e i ast -- парсер элементарного выражения
  -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
  -> (op -> ast -> ast) -- конструктор узла для унарной операции
  -> Parser e i ast
uberExpr [] ast binary unary = ast
uberExpr ((p, Binary LeftAssoc):pa) ast binary unary = do
  (op, terms) <- sepBy1' p (uberExpr pa ast binary unary)
  return $ foldl (\acc (op, term) -> binary op acc term) op terms
uberExpr ((p, Binary RightAssoc):pa) ast binary unary = do
  (terms, op) <- sepBy1'' p (uberExpr pa ast binary unary)
  return $ foldr (\(term, op) acc -> binary op term acc) op terms
uberExpr ((p, Binary NoAssoc):pa) ast binary unary = parse <|> uberExpr pa ast binary unary
  where
    parse = do
      lhs <- uberExpr pa ast binary unary
      op <- p
      binary op lhs <$> uberExpr pa ast binary unary
uberExpr ((p, Unary):pa) ast binary unary = parse <|> uberExpr pa ast binary unary
  where
    parse = unary <$> p <*> uberExpr pa ast binary unary <|> unary <$> p <*> ast
