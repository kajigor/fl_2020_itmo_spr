module UberExpr where

import           Combinators (Parser (..))
import           Control.Applicative

data Associativity = LeftAssoc | RightAssoc | NoAssoc

uberExpr :: Monoid e
         => [(Parser e i op, Associativity)]
         -> Parser e i ast
         -> (op -> ast -> ast -> ast)
         -> Parser e i ast
uberExpr table child cont =
  foldr step child table
  where
    step (opParser, LeftAssoc)  child = do
        c <- child
        foldl (\x (op, t) -> cont op x t) c <$>
          many ((,) <$> opParser <*> child)

    step (opParser, RightAssoc) child = go
      where
        go = do
          c <- child
          (do
              op <- opParser
              nxt <- go
              pure $ cont op c nxt
            ) <|> pure c

    step (opParser, NoAssoc)    child =
      (flip cont <$> child <*> opParser <*> child) <|> child
