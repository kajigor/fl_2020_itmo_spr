module UberExpr where

import           Combinators         (Parser (..), sepBy1', sepBy1'')
import           Control.Applicative ((<|>))

data Associativity
  = LeftAssoc
  | RightAssoc
  | NoAssoc

-- Есть парсер +, *
-- elementaryExpr - парсер термов
-- astConstructor - \op left right -> BinOp op left right
-- 4*3+5*6
-- 4 * 3 * 5
-- 3 < 4
uberExpr ::
     Monoid e => [(Parser e i op, Associativity)] -> Parser e i ast -> (op -> ast -> ast -> ast) -> Parser e i ast
uberExpr [(opParser, assoc)] elementaryExprParser astConstructor =
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
    convert (Nothing, v) _ = (Nothing, v)
uberExpr ((opParser, assoc):xs) elementaryExprParser astConstructor =
  case assoc of
    LeftAssoc ->
      uncurry (foldl (\ast (op, ast') -> astConstructor op ast ast')) <$>
      sepBy1' opParser (uberExpr xs elementaryExprParser astConstructor)
    RightAssoc -> createRightAssoc <$> sepBy1'' opParser (uberExpr xs elementaryExprParser astConstructor)
    NoAssoc -> binOp <|> single
      where binOp = do
              left <- uberExpr xs elementaryExprParser astConstructor
              op <- opParser
              astConstructor op left <$> uberExpr xs elementaryExprParser astConstructor
            single = uberExpr xs elementaryExprParser astConstructor
  where
    createRightAssoc xs = snd $ foldr1 (\val (_, acc) -> convert val acc) xs
    convert (Just sep, v) acc = (Just sep, astConstructor sep v acc)
    convert (Nothing, v) _    = (Nothing, v)
