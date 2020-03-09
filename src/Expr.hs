module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail', satisfy, symbol, sepBy1)
import           Data.Char   (isDigit, digitToInt)
import           Control.Applicative
import           Data.Functor ((<&>))
import           Data.Foldable (asum)
import           UberExpr

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = uberExpr
  [ (symbol '/' *> pure Div <|> symbol '*' *> pure Mult, LeftAssoc) ]
  parseTerm
  BinOp


-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = uberExpr
  [ (symbol '+' *> pure Plus <|> symbol '-' *> pure Minus, LeftAssoc) ]
  parseMult
  BinOp

-- Парсер чисел
parseNum :: Parser String String Int
parseNum =
  fmap toNum go
  where
    digit = satisfy isDigit
    toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
    go = some digit

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = pure Plus
toOperator '*' = pure Mult
toOperator '-' = pure Minus
toOperator '/' = pure Div
toOperator _   = fail' "Failed toOperator"

-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTerm :: Parser String String AST
parseTerm =
    fmap Num parseNum <|>
    (do
        lbr
        e <- parseSum
        rbr
        pure e
    )
  where
    lbr = symbol '('
    rbr = symbol ')'

-- Парсер арифметических выражений над целыми числами с операциями +,-,*,/.
parseExpr :: Parser String String AST
parseExpr = parseSum

compute :: AST -> Int
compute (Num x) = x
compute (BinOp Plus x y) = compute x + compute y
compute (BinOp Mult x y) = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y) = compute x `div` compute y

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _ -> Nothing
