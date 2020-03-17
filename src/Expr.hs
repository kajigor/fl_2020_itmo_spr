module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..))
import           Data.Char   (isDigit, digitToInt)

-- Парсер арифметических выражений над целыми числами
parseExpr :: Parser String String AST
parseExpr = error "parseExpr undefined"

parseIdent :: Parser String String String
parseIdent = error "parseIdent undefined"

-- Парсер чисел
parseNum :: Parser String String Int
parseNum = error "parseNum undefined"

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = error "parseOp undefined"

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