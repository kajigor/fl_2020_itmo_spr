module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..),  elem', fail', 
                              return', satisfy, symbol, sepBy1, sepBy1', many', some')
import           Data.Char   (isDigit, digitToInt)
import           Control.Applicative (Alternative (..))
import           UberExpr            (Associativity (..), uberExpr)
import           Data.Char



parseSum :: Parser String String AST
parseSum = uberExpr [(parseAddOp <|> parseSubOp, LeftAssoc), (parseMultOp <|> parseDivOp, LeftAssoc)]
           (Num <$> parseNum <|> symbol '(' *> parseSum <* symbol ')')
           BinOp
           where
               parseMultOp = symbol '*' >>= toOperator
               parseAddOp = symbol '+' >>= toOperator
               parseSubOp = symbol '-' >>= toOperator
               parseDivOp = symbol '/' >>= toOperator


parseMult :: Parser String String AST
parseMult = uberExpr [(parseMultOp <|> parseDivOp, LeftAssoc)]
           (Num <$> parseNum <|> symbol '(' *> parseSum <* symbol ')')
           BinOp
           where
               parseMultOp = symbol '*' >>= toOperator
               parseDivOp = symbol '/' >>= toOperator



parseIdent :: Parser String String String
parseIdent = (:) <$> parseLetter <*> many ( parseLetter <|> satisfy isDigit )
    where
        parseLetter = satisfy isLetter <|> symbol '_'

-- Парсер чисел
parseNum :: Parser String String Int
parseNum =
     (toNum <$> go) <|> (negate <$> (toNum <$> (symbol '-' >> go)))
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
toOperator _   = fail' "Failed toOperator"


toOperatorStr :: String -> Parser String String Operator
toOperatorStr "||" = return Or
toOperatorStr "&&" = return And
toOperatorStr "==" = return Eq
toOperatorStr "/=" = return Neq
toOperatorStr ">=" = return Geq
toOperatorStr "<=" = return Leq
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
parseExpr =  uberExpr [(parseOrOp, RightAssoc),
                     (parseAndOp, RightAssoc),
                     (parseGeqOp <|> parseLeqOp <|> parseLtOp <|> parseGtOp <|> parseEqOp <|> parseNeqOp, NoAssoc),
                     (parseAddOp <|> parseSubOp, LeftAssoc),
                     (parseMultOp <|> parseDivOp, LeftAssoc),
                     (parsePowOp, RightAssoc)]
           (Num <$> parseNum <|> symbol '(' *> parseExpr <* symbol ')' <|> Ident <$> parseIdent)
           BinOp
           where
               parseMultOp = symbol '*' >>= toOperator
               parseAddOp = symbol '+' >>= toOperator
               parseSubOp = symbol '-' >>= toOperator
               parseDivOp = symbol '/' >>= toOperator
               parsePowOp = symbol '^' >>= toOperator
               parseLtOp = symbol '<' >>= toOperator
               parseGtOp = symbol '>' >>= toOperator
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
 


