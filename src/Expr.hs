module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators 
import           Control.Applicative (Alternative (..))    
import           Data.Char   (isDigit, digitToInt)

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = do
   (n, ops) <- sepBy1' parseDivOrMult parseTerm
   return $ foldl (\acc (op, term) -> BinOp op acc term) n ops
  

-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = do
   (n, ops) <- sepBy1' parsePlusOrMinus parseMult
   return $ foldl (\acc (op, term) -> BinOp op acc term) n ops

parsePlusOrMinus = do
  op <- parseOp
  if op == Plus || op == Minus then return op else empty

parseDivOrMult = do
  op <- parseOp
  if op == Div || op == Mult then return op else empty


-- Парсер чисел
parseNum :: Parser String String Int
parseNum = fmap (\l -> foldl (\acc a -> acc * 10 + (digitToInt a)) 0 l) parseNumStr where
  parseNumStr = do
        digit <- satisfy isDigit
        number <- parseNumStr <|> (return "")
        return $ digit:number

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

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
parseTerm = (fmap Num parseNum) <|> (lbr *> parseExpr <* rbr)
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