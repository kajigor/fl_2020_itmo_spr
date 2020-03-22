module UberExpr where

import           Combinators         (Parser (..), sepBy1', sepBy1'')
import           Control.Applicative ((<|>))
import           Debug.Trace

data Associativity
  = LeftAssoc
  | RightAssoc
  | NoAssoc

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