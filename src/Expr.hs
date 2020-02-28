module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail', satisfy, symbol, sepBy1)
import           Data.Char   (isDigit, digitToInt)
import           Control.Applicative

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = parseDiv
  where
    parseDiv =
      foldr1 (BinOp Div) <$> sepBy1 (symbol '/') parseProd
    parseProd =
      foldr1 (BinOp Mult) <$> sepBy1 (symbol '*') parseTerm

-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = parseMinus
  where
    parsePlus =
      foldr1 (BinOp Plus) <$> sepBy1 (symbol '+') parseMult
    parseMinus =
      foldr1 (BinOp Minus) <$> sepBy1 (symbol '-') parsePlus

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
