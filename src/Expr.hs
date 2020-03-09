module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), alt, elem', fail', map',
                              return', satisfy, seq', symbol, empty', sepBy1, some')
import           Data.Char   (isDigit, digitToInt)

sepByOp :: Parser String String Operator -> Parser String String AST -> Parser String String [(Operator, AST)]
sepByOp sep elem = prsr `alt` prsr'
  where prsr = do
          el <- elem
          op <- sep
          elems <- sepByOp sep elem
          return ((op, el):elems)
        prsr' = do
          el <- elem
          return [(None, el)]

folder :: (Operator, AST) -> (Operator, AST) -> (Operator, AST)
folder (op, ast) (op', ast') = (op', BinOp op ast ast')

parseMult :: Parser String String AST
parseMult = do
      lst <- sepByOp parseMultDiv parseTerm
      let (_, ans) = foldl1 folder lst
      return ans

parseSum :: Parser String String AST
parseSum = do
      lst <- sepByOp parseOp parseMult
      let (_, ans) = foldl1 folder lst
      return ans

-- Парсер чисел
parseNum :: Parser String String Int
parseNum =
    map' toNum go
  where
    digit = satisfy isDigit
    empty' = return' []
    toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
    go = do
      d <- digit
      map' (d:) (go `alt` empty')

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

-- Не уверен считается ли это сравнением с образцом, но иначе не знаю
-- как сделать функцию parseMult.
parseMultDiv :: Parser String String Operator
parseMultDiv = satisfy (\x -> x == '/' || x == '*') >>= toOperator

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
parseTerm = map' Num parseNum `alt` do
      symbol '('
      e <- parseSum
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