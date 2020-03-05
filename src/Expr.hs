module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), symbol, satisfy, elem', fail', sepBy1, sepBy1')
import           Data.Char   (isDigit, digitToInt)
import           Control.Applicative (Alternative (..))

-- Парсер для произведения/деления термов
-- 3*4*2
parseMult :: Parser String String AST
parseMult = (\(l, r) -> foldl (\acc (op, term) -> BinOp op acc term) l r) <$> sepBy1' (parseOp' '*' <|> parseOp' '/') parseTerm


-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = (\(l, r) -> foldl (\acc (op, term) -> BinOp op acc term) l r) <$> sepBy1' (parseOp' '+' <|> parseOp' '-') parseMult

parseNum :: Parser String String Int
parseNum = read <$> parseAsString where
        parseAsString = do digit <- satisfy isDigit
                           num <- parseAsString <|> (return "")
                           return $ (digit:num)

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

parseOp' :: Char -> Parser String String Operator
parseOp' c = symbol c >>= toOperator

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
parseTerm = Num <$> parseNum <|> parseBracketed where
  parseBracketed = do symbol '('
                      e <- parseExpr
                      symbol ')'
                      return e

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