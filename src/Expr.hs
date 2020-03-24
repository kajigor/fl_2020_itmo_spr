module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), sepBy1', elem', satisfy, symbol, multi_symbol, fail', many')
import           Data.Char   (isDigit, isLetter, digitToInt)
import           Control.Applicative (empty, (<|>))
import           UberExpr    

parseMultOp :: Parser String String Operator	
parseMultOp = (symbol '*' <|> symbol '/') >>= toOperator
		
parseSumOp :: Parser String String Operator	
parseSumOp = (symbol '+' <|> symbol '-') >>= toOperator

parsePowOp :: Parser String String Operator 
parsePowOp = (symbol '^') >>= toOperator

parseEqOp :: Parser String String Operator 
parseEqOp = (multi_symbol "==") >>= toOperator'

parseNeqOp :: Parser String String Operator 
parseNeqOp = (multi_symbol "/=") >>= toOperator'

parseLeqOp :: Parser String String Operator 
parseLeqOp = (multi_symbol "<=") >>= toOperator' 

parseLtOp :: Parser String String Operator 
parseLtOp = (symbol '<') >>= toOperator 

parseGeqOp :: Parser String String Operator 
parseGeqOp = (multi_symbol ">=") >>= toOperator'

parseGtOp :: Parser String String Operator 
parseGtOp = (symbol '>') >>= toOperator 

parseAndOp :: Parser String String Operator 
parseAndOp = (multi_symbol "&&")  >>= toOperator'

parseOrOp :: Parser String String Operator 
parseOrOp = (multi_symbol "||")  >>= toOperator'

parseIdent :: Parser String String String
parseIdent = do
            letter <- satisfy isLetter <|> symbol '_'
            others <- many' (satisfy isLetter <|> satisfy isDigit <|> symbol '_')
            return (letter:others)

-- Парсер чисел
parseNumHelper :: Parser String String Int
parseNumHelper =
    fmap toNum go 
  where
    digit = satisfy isDigit
    empty' = return []
    toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
    go =
      digit >>= \d -> fmap (d:) (go <|> empty')

parseNum:: Parser String String Int
parseNum = do
    minus_list <- many' (symbol '-')
    number <- parseNumHelper
    if even (length minus_list) then return number else return (negate number)

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = return Plus
toOperator '*' = return Mult
toOperator '-' = return Minus
toOperator '/' = return Div
toOperator '^' = return Pow
toOperator '<' = return Lt
toOperator '>' = return Gt
toOperator _   = fail' "Failed toOperator"

toOperator' :: String -> Parser String String Operator
toOperator' "==" = return Equal
toOperator' "/=" = return Nequal
toOperator' "<=" = return Le
toOperator' ">=" = return Ge
toOperator' "&&" = return And
toOperator' "||" = return Or
toOperator' _   = fail' "Failed toOperator"

-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTerm :: Parser String String AST
parseTerm = (fmap Num parseNum) <|> (fmap Ident parseIdent) <|> parseBrackets
	where parseBrackets = do
		symbol '('
		x <- parseExpr
		symbol ')'
		return x

-- Парсер арифметических выражений над целыми числами с операциями +,-,*,/.
parseExpr :: Parser String String AST
parseExpr = uberExpr [(parseOrOp, RightAssoc),
                      (parseAndOp, RightAssoc),
                      (parseEqOp <|> parseNeqOp <|> parseLeqOp <|> parseLtOp <|> parseGeqOp <|> parseGtOp, NoAssoc),
                      (parseSumOp, LeftAssoc),
                      (parseMultOp, LeftAssoc), 
                      (parsePowOp, RightAssoc)] 
                      parseTerm BinOp


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
