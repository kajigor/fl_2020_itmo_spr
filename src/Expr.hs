module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..),  elem', fail', 
                              return', satisfy, symbol, sepBy1, sepBy1', many', some')
import           Data.Char   (isDigit, digitToInt)
import           Control.Applicative (Alternative (..))
import           UberExpr            (Associativity (..), OpType (..), uberExpr)
import           Data.Char



parseSum :: Parser String String AST
parseSum = uberExpr [(parseAddOp <|> parseSubOp, Binary LeftAssoc), (parseMultOp <|> parseDivOp, Binary LeftAssoc)]
           (Num <$> parseNum <|> symbol '(' *> parseSum <* symbol ')')
           BinOp
           UnaryOp
           where
               parseMultOp = symbol '*' >>= toOperator
               parseAddOp = symbol '+' >>= toOperator
               parseSubOp = symbol '-' >>= toOperator
               parseDivOp = symbol '/' >>= toOperator


parseMult :: Parser String String AST
parseMult = uberExpr [(parseMultOp <|> parseDivOp, Binary LeftAssoc)]
           (Num <$> parseNum <|> symbol '(' *> parseSum <* symbol ')')
           BinOp
           UnaryOp
           where
               parseMultOp = symbol '*' >>= toOperator
               parseDivOp = symbol '/' >>= toOperator



parseIdent :: Parser String String String
parseIdent = (:) <$> parseLetter <*> many ( parseLetter <|> satisfy isDigit )
    where
        parseLetter = satisfy isLetter <|> symbol '_'

-- Парсер чисел
parseNum :: Parser String String Int
parseNum = toNum <$> go
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
toOperator '^' = return Pow
toOperator '<' = return Lt
toOperator '>' = return Gt
toOperator '!' = return Not
toOperator _   = fail' "Failed toOperator"


toOperatorStr :: String -> Parser String String Operator
toOperatorStr "||" = return Or
toOperatorStr "&&" = return And
toOperatorStr "==" = return Equal
toOperatorStr "/=" = return Nequal
toOperatorStr ">=" = return Ge
toOperatorStr "<=" = return Le
toOperatorStr _   = fail' "Failed toOperator"

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
parseExpr =  uberExpr [(parseOrOp, Binary RightAssoc),
                     (parseAndOp, Binary RightAssoc),
                     (parseNotOp, Unary),
                     (parseGeqOp <|> parseLeqOp <|> parseLtOp <|> parseGtOp <|> parseEqOp <|> parseNeqOp, Binary NoAssoc),
                     (parseAddOp <|> parseSubOp, Binary LeftAssoc),
                     (parseMultOp <|> parseDivOp, Binary LeftAssoc),
                     (parseSubOp, Unary),
                     (parsePowOp, Binary RightAssoc)]
           (Num <$> parseNum <|> symbol '(' *> parseExpr <* symbol ')' <|> Ident <$> parseIdent)
           BinOp
           UnaryOp
           where
               parseMultOp = symbol '*' >>= toOperator
               parseAddOp = symbol '+' >>= toOperator
               parseSubOp = symbol '-' >>= toOperator
               parseDivOp = symbol '/' >>= toOperator
               parsePowOp = symbol '^' >>= toOperator
               parseLtOp = symbol '<' >>= toOperator
               parseGtOp = symbol '>' >>= toOperator
               parseNotOp = symbol '!' >>= toOperator
               parseOrOp = (:) <$> (symbol '|') <*> ((:[]) <$> (symbol '|')) >>= toOperatorStr
               parseAndOp = (:) <$> (symbol '&') <*> ((:[]) <$> (symbol '&')) >>= toOperatorStr
               parseLeqOp = (:) <$> (symbol '<') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr
               parseGeqOp = (:) <$> (symbol '>') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr
               parseEqOp = (:) <$> (symbol '=') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr
               parseNeqOp = (:) <$> (symbol '/') <*> ((:[]) <$> (symbol '=')) >>= toOperatorStr


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
 


