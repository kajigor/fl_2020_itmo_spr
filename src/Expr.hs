module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', satisfy, symbol, fail')
import           Data.Char   (isDigit, digitToInt)
import           Control.Applicative (empty, (<|>))

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = parse <|> parseTerm
	where parse = do
		x <- parseTerm
		op <- parseMultOp
		xs <- parseMult
		return (BinOp op x xs)

parseMultOp :: Parser String String Operator	
parseMultOp = (symbol '*' <|> symbol '/') >>= toOperator

-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = parse <|> parseMult
	where parse = do
		x <- parseMult
		op <- parseSumOp
		xs <- parseSum
		return (BinOp op x xs)

parseSumOp :: Parser String String Operator	
parseSumOp = (symbol '+' <|> symbol '-') >>= toOperator

-- Парсер чисел
parseNum :: Parser String String Int
parseNum =
    fmap toNum go
  where
    digit = satisfy isDigit
    empty' = return []
    toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
    go =
      digit >>= \d -> fmap (d:) (go <|> empty')

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
parseTerm = (fmap Num parseNum) <|> parseBrackets 
	where parseBrackets = do
		symbol '('
		x <- parseExpr
		symbol ')'
		return x

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
