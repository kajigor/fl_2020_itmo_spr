module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), alt, elem', fail', map',
                              return', satisfy, seq', symbol, sepBy1)
import           Data.Char   (isDigit, digitToInt)

import           Control.Applicative (Alternative (..))

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = let parseMultOp = (symbol '*' <|> symbol '/') >>= toOperator in
  ((\x f y -> BinOp f x y) <$> parseTerm <*> parseMultOp <*> parseMult <|> parseTerm) <|> parseTerm

-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = let parseSumOp = (symbol '+' <|> symbol '-') >>= toOperator in
  ((\x f y -> BinOp f x y) <$> parseMult <*> parseSumOp <*> parseSum <|> parseMult) <|> parseMult

-- Парсер чисел
parseNum :: Parser String String Int
parseNum =
    map' toNum go
  where
    digit = satisfy isDigit
    empty' = return' []
    toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
    go =
      digit `seq'`
      \d -> map' (d:) (go `alt` empty')

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' `seq'` toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = return' Plus
toOperator '*' = return' Mult
toOperator '-' = return' Minus
toOperator '/' = return' Div
toOperator _   = fail' "Failed toOperator"

-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTerm :: Parser String String AST
parseTerm =
    map' Num parseNum `alt`
    (lbr `seq'` \_ ->
     parseTerm `seq'` \e ->
     rbr `seq'` \_ ->
     return' e
    ) `alt`
    (lbr `seq'` \_ ->
     parseSum `seq'` \e ->
     rbr `seq'` \_ ->
     return' e
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
