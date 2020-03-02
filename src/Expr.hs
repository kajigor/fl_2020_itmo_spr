module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..),  elem', fail', 
                              return', satisfy, symbol, sepBy1, many', some')
import           Data.Char   (isDigit, digitToInt)
import           Control.Applicative (Alternative (..))

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = parseM <|> parseTerm
             where parseM = do
                         term <- parseTerm
                         op <- parseMulDivOp
                         mul <- parseMult
                         return $ BinOp op term mul
                   parseMulDivOp = (mul <|> div) >>= toOperator
                   mul = symbol '*'
                   div = symbol '/'


 -- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = parseS <|> parseMult
            where parseS = do
                         mul <- parseMult
                         op <- parseAddSubOp
                         sum <- parseSum
                         return $ BinOp op mul sum
                  parseAddSubOp = (add <|> sub) >>= toOperator
                  sub = symbol '-'
                  add = symbol '+'


-- Парсер чисел
parseNum :: Parser String String Int
parseNum =
     toNum <$> go
   where
     digit = satisfy isDigit
     empty' = return []
     toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
     go = do
       d <- digit
       (d:) <$> (go <|> empty')

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
parseTerm = Num <$> parseNum <|> parseBr
     where parseBr = do 
                   lbr
                   result <- parseSum
                   rbr
                   return $ result 
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
          Success rest ast -> if null rest then return $ compute ast else Nothing
          _ -> Nothing
 
