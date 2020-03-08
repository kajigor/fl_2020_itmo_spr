module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), alt, fail',
                              elem', satisfy, symbol, sepBy1)
import           UberExpr
import           Data.Char   (isDigit, digitToInt)
import           Control.Applicative (Alternative (..))

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = let
    mult = symbol '*' >>= toOperator
    div' = symbol '/' >>= toOperator
  in uberExpr [(mult, RightAssoc), (div', LeftAssoc)]
              parseTerm
              BinOp

-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = let
    plus = symbol '+' >>= toOperator
    minus = symbol '-' >>= toOperator
  in uberExpr [(plus, RightAssoc), (minus, LeftAssoc)]
              parseMult
              BinOp

-- Парсер чисел
parseNum :: Parser String String Int
parseNum = toNum <$> some digit where
  toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
  digit = satisfy isDigit

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = return Plus
toOperator '*' = return Mult
toOperator '-' = return Minus
toOperator '/' = return Div
toOperator _   = fail' "Failed toOperator"

-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTerm :: Parser String String AST
parseTerm = Num <$> parseNum <|> (lbr *> parseSum <* rbr)
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
