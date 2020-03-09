module UberExpr where

import           Combinators         (Parser (..), sepBy1', sepBy1'')
import           Control.Applicative ((<|>))

data Associativity
  = LeftAssoc
  | RightAssoc
  | NoAssoc

uberExprHelper (opParser, assoc) elementaryExprParser astConstructor =
  case assoc of
    LeftAssoc ->
      uncurry (foldl (\ast (op, ast') -> astConstructor op ast ast')) <$> sepBy1' opParser elementaryExprParser
    RightAssoc -> createRightAssoc <$> sepBy1'' opParser elementaryExprParser
    NoAssoc -> binOp <|> single
      where binOp = do
              left <- elementaryExprParser
              op <- opParser
              astConstructor op left <$> elementaryExprParser
            single = elementaryExprParser
  where
    createRightAssoc xs = snd $ foldr1 (\val (_, acc) -> convert val acc) xs
    convert (Just sep, v) acc = (Just sep, astConstructor sep v acc)
    convert (Nothing, v) _    = (Nothing, v)

uberExpr xs@[(opParser, assoc)] elementaryExprParser astConstructor =
  uberExprHelper (opParser, assoc) elementaryExprParser astConstructor
uberExpr ((opParser, assoc):xs) elementaryExprParser astConstructor =
  uberExprHelper (opParser, assoc) (uberExpr xs elementaryExprParser astConstructor) astConstructor
