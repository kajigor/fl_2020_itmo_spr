module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), InputStream (..), elem', satisfy, symbol, symbols, runParser, fail')
import           Data.Char   (isDigit, digitToInt)
import           UberExpr    (uberExpr, foldl1', Associativity (..), OpType (..))

import Control.Applicative
import qualified Data.Map as Map

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
parseOp = (elem' >>= toOperator) <|> do
    char <- elem'
    char2 <- elem'
    toOperator' (char:char2:[])

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = return Plus
toOperator '*' = return Mult
toOperator '-' = return Minus
toOperator '/' = return Div
toOperator '^' = return Pow
toOperator '!' = return Not
toOperator _   = fail' "Failed toOperator"

toOperator' :: String -> Parser String String Operator
toOperator' "==" = return Equal
toOperator' "/=" = return Nequal
toOperator' "<=" = return Le
toOperator' "<"  = return Lt
toOperator' ">=" = return Ge 
toOperator' ">"  = return Gt
toOperator' "&&" = return And
toOperator' "||" = return Or
toOperator' _     = fail' "Failed toOperator'"

mult  = symbol '*' >>= toOperator
sum'  = symbol '+' >>= toOperator
minus = symbol '-' >>= toOperator
div'  = symbol '/' >>= toOperator
pow   = symbol '^' >>= toOperator
not'  = symbol '!' >>= toOperator
eq    = symbols "=="  >>= toOperator'
neq   = symbols "/=" >>= toOperator'
leq   = symbols "<="  >>= toOperator'
lt    = symbols "<"   >>= toOperator'
geq   = symbols ">="  >>= toOperator'
gt    = symbols ">"   >>= toOperator'
and'  = symbols "&&"  >>= toOperator'
or'   = symbols "||"  >>= toOperator'

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

falsy :: Int
falsy = 0

truthy :: Int
truthy = 1 -- or any other non-zero value

fromBool op x y | x `op` y = truthy
                | otherwise = falsy

evalExpr :: AST -> Map.Map String Int -> Maybe Int
evalExpr (Num x) _ = return x
evalExpr (Ident x) env = Map.lookup x env

evalExpr (UnaryOp Minus x) env = do
    value <- evalExpr x env
    return $ -value

evalExpr (UnaryOp Not x) env = do
        value <- evalExpr x env
        if value == falsy
            then return truthy
            else return falsy

evalExpr (BinOp op x y) env = do
          x' <- evalExpr x env
          y' <- evalExpr y env
          return $ case op of 
                Plus -> x' + y'
                Mult -> x' * y'
                Minus -> x' - y'
                Div -> x' `div` y'
                Pow -> x' ^ y'
                Equal -> fromBool (==) x' y'
                Nequal -> fromBool (/=) x' y'
                Gt -> fromBool (>) x' y'
                Lt -> fromBool (<) x' y'
                Ge -> fromBool (>=) x' y'
                Le -> fromBool (<=) x' y'
                And -> if x' == falsy then falsy else y'
                Or -> if x' == falsy then y' else truthy
                _ -> error "evalExpr undefined"
 

evaluate :: String -> Maybe Int
evaluate input = do 
     case runParser parseExpr input of
          Success rest msg ast | null (stream rest) -> return $ compute ast 
          _ -> Nothing

