module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), InputStream (..), elem', satisfy, symbol, symbols, runParser)
import           Data.Char   (isDigit, digitToInt)
import           UberExpr    (uberExpr, foldl1', Associativity (..), OpType (..))

import Control.Applicative

fullAlphabet :: [Char]
fullAlphabet = symbolsOfAlphabet ++ digitsOfAlphabet ++ suffixCharsOfAlphabet

symbolsOfAlphabet :: [Char]
symbolsOfAlphabet =  ['A'..'Z'] ++ ['a'..'z'] ++ ['_']

digitsOfAlphabet :: [Char]
digitsOfAlphabet = ['0'..'9']

suffixCharsOfAlphabet :: [Char]
suffixCharsOfAlphabet = ['\'']

ofList :: [Char] -> Parser String String Char
ofList list = satisfy (\x -> x `elem` list)

parseIdent :: Parser String String String
parseIdent = noDigitsAtTheStart >>= whateverLetter >>= ticksAtTheEnd
    where noDigitsAtTheStart = some (ofList symbolsOfAlphabet)
          whateverLetter = \x -> do
                rest <- many (ofList (symbolsOfAlphabet ++ digitsOfAlphabet))
                return $ x ++ rest
          ticksAtTheEnd = \x -> do
                ticks <- many (ofList suffixCharsOfAlphabet)
                return $ x ++ ticks
          
-- Парсер чисел
parseNum :: Parser String String Int
parseNum = parseNatural <|> (symbol '-' *> (negate <$> parseNum))

parseNatural :: Parser String String Int
parseNatural =
    toNum <$> go
  where
    digit = satisfy isDigit
    empty = return []
    toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
    go = do
      d <- digit
      (d:) <$> (go <|> empty)

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
toOperator '!' = return Not
toOperator _   = fail "Failed toOperator"

toOp :: String -> Parser String String Operator
toOp "==" = return Equal
toOp "/=" = return Nequal
toOp "<=" = return Le
toOp "<"  = return Lt
toOp ">=" = return Ge 
toOp ">"  = return Gt
toOp "&&" = return And
toOp "||" = return Or
toOp _     = fail "Failed toOp"

mult  = symbol '*' >>= toOperator
sum'  = symbol '+' >>= toOperator
minus = symbol '-' >>= toOperator
div'  = symbol '/' >>= toOperator
pow   = symbol '^' >>= toOperator
not'  = symbol '!' >>= toOperator
eq    = symbols "=="  >>= toOp
neq   = symbols "/=" >>= toOp
leq   = symbols "<="  >>= toOp
lt    = symbols "<"   >>= toOp
geq   = symbols ">="  >>= toOp
gt    = symbols ">"   >>= toOp
and'  = symbols "&&"  >>= toOp
or'   = symbols "||"  >>= toOp

-- Парсер арифметических выражений над целыми числами с арифметическими операциями +,-,*,/,^ и бинарными логическими операциями.
parseExpr :: Parser String String AST
parseExpr =
     uberExpr [ (or', Binary RightAssoc)
              , (and', Binary RightAssoc)
              , (not', Unary)
              , (eq <|> neq <|> leq <|> lt <|> geq <|> gt, Binary NoAssoc)
              , (sum' <|> minus, Binary LeftAssoc)
              , (mult <|> div', Binary LeftAssoc)
              , (minus, Unary)
              , (pow, Binary RightAssoc)
              ] 
              ((Num <$> parseNatural) <|> (Ident <$> parseIdent) <|> (symbol '(' *> parseExpr <* symbol ')') )
              BinOp
              UnaryOp

compute :: AST -> Int
compute (Num x) = x
compute (BinOp Plus x y) = compute x + compute y
compute (BinOp Mult x y) = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y) = compute x `div` compute y
compute (BinOp Pow x y) = compute x ^ compute y
compute (UnaryOp Minus x) = 0 - (compute x)
compute _ = error "compute undefined"

truthy = 1
falsy = 0
-- 0 is false
evalExpr :: AST -> Int
evalExpr (BinOp Equal x y) | compute x == compute y = truthy
                           | otherwise = falsy 

evalExpr (BinOp Nequal x y) | compute x /= compute y = truthy
                            | otherwise = falsy
                            
evalExpr (BinOp Gt x y) | compute x > compute y = truthy
                        | otherwise = falsy

evalExpr (BinOp Lt x y) | compute x < compute y = truthy
                        | otherwise = falsy

evalExpr (BinOp Ge x y) | compute x >= compute y = truthy
                        | otherwise = falsy

evalExpr (BinOp Le x y) | compute x <= compute y = truthy
                        | otherwise = falsy

evalExpr (BinOp And x y) | (evalExpr x == truthy) && (evalExpr y == truthy) = truthy
                         | otherwise = falsy

evalExpr (BinOp Or x y) | (evalExpr x == truthy) || (evalExpr y == truthy) = truthy
                        | otherwise = falsy

evalExpr (UnaryOp Not x) | evalExpr x == falsy = truthy
                         | otherwise = falsy
                       
evalExpr _ = error "evalExpr undefined"

evaluate :: String -> Maybe Int
evaluate input = do 
     case runParser parseExpr input of
          Success rest msg ast | null (stream rest) -> return $ compute ast 
          _ -> Nothing

