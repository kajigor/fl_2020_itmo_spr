module Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), elem', nat,
                                      satisfy, sepBy1', symbol)
import           Control.Applicative
import           Data.Char           (digitToInt, isDigit)
import           UberExpr            (Associativity (..), uberExpr)

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = uberExpr [(parseMultDivOp, LeftAssoc)] parseTerm BinOp
  where
    parseMultDivOp = (symbol '*' <|> symbol '/') >>= toOperator

-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = uberExpr [(parseSumSub, LeftAssoc)] parseMult BinOp
  where
    parseSumSub = (symbol '+' <|> symbol '-') >>= toOperator

-- Парсер чисел
parseNum :: Parser String String Int
parseNum = nat

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

fail' :: e -> Parser e i a
fail' e = Parser $ \input -> Failure e

-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTerm :: Parser String String AST
parseTerm = Num <$> parseNum <|> (parseLeftBracket *> parseSum <* parseRightBracket)
  where
    parseLeftBracket = symbol '('
    parseRightBracket = symbol ')'

-- Парсер арифметических выражений над целыми числами с операциями +,-,*,/.
parseExpr :: Parser String String AST
parseExpr = parseSum

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y

evaluate :: String -> Maybe Int
evaluate input =
  case runParser parseExpr input of
    Success rest ast
      | null rest -> return $ compute ast
    _ -> Nothing
