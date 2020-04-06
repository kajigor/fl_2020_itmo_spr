module UberExpr where

import           Combinators         (Parser (..), sepBy1', sepBy1'')
import           Control.Applicative ((<|>))
import           Debug.Trace

data Associativity
  = LeftAssoc
  | RightAssoc
  | NoAssoc

data OpType = Binary Associativity
            | Unary

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr = error "uberExpr undefined"


uberExpr ::
     (Monoid e, Show ast, Show op)
  => [(Parser e i op, Associativity)]
  -> Parser e i ast
  -> (op -> ast -> ast -> ast)
  -> Parser e i ast
uberExpr [] ast _ = ast
uberExpr ((p, LeftAssoc):pa) ast builder = do
  (op, terms) <- sepBy1' p (uberExpr pa ast builder)
  return $ foldl (\acc (op, term) -> builder op acc term) op terms
uberExpr ((p, RightAssoc):pa) ast builder = do
  (terms, op) <- sepBy1'' p (uberExpr pa ast builder)
  return $ foldr (\(term, op) acc -> builder op term acc) op terms
uberExpr ((p, NoAssoc):pa) ast builder = parse <|> uberExpr pa ast builder
  where
    parse = do
      lhs <- uberExpr pa ast builder
      op <- p
      builder op lhs <$> uberExpr pa ast builder