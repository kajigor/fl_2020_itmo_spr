module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), alt, elem', fail', map',
                              return', satisfy, seq', symbol)
import           Data.Char   (isDigit, digitToInt)
import           Control.Applicative (Alternative (..))

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = op <|> num where
  op = do num <- parseNum
          op <- parseOp
          case op of
            m | m == Mult || m == Div -> do rest <- parseMult
                                            return $ BinOp m (Num num) rest
            _ -> empty

  num = Num <$> parseNum

-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = error "parseSum not implemented"

parseNum :: Parser String String Int
parseNum = read <$> parseAsString where
        parseAsString = do digit <- satisfy isDigit
                           num <- parseAsString <|> (return "")
                           return $ (digit:num)

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
parseTerm =
    map' Num parseNum `alt`
    (lbr `seq'` \_ ->
     parseTerm `seq'` \e ->
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