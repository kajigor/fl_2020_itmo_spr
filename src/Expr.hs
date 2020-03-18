module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators 
import           Data.Char   (isDigit, digitToInt)
import UberExpr
import Control.Applicative (Alternative (..))  

sum'  = symbol '+' >>= toOperator
minus = symbol '-' >>= toOperator
mult  = symbol '*' >>= toOperator
div'  = symbol '/' >>= toOperator

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = uberExpr [(mult <|> div', LeftAssoc)] parseTerm BinOp

-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = uberExpr [ (sum' <|> minus, LeftAssoc), (mult <|> div', LeftAssoc)
                    ]
                    parseTerm
                    BinOp

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
parseTerm =
    fmap Num parseNum <|>
    (lbr >>= \_ ->
     parseExpr >>= \e ->
     rbr >>= \_ ->
     return e
    )
  where
    lbr = symbol '('
    rbr = symbol ')'

-- Парсер арифметических выражений над целыми числами с операциями +,-,*,/.
parseExpr :: Parser String String AST
parseExpr = parseSum

parseNum = fmap (\l -> foldl (\acc a -> acc * 10 + (digitToInt a)) 0 l) parseNumStr where
  parseNumStr = do
        digit <- satisfy isDigit
        number <- parseNumStr <|> (return "")
        return $ digit:number

compute :: AST -> Int
compute (Num x) = x
compute (BinOp Plus x y) = compute x + compute y
compute (BinOp Mult x y) = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y) = compute x `div` compute y
compute _ = error "compute undefined"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _ -> Nothing