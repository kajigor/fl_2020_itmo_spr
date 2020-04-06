module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), alt, elem', fail', map',
                              return', satisfy, symbol, empty', many', some', symbols)
import           Data.Char   (isDigit, digitToInt)
import           UberExpr    (uberExpr, foldl1', Associativity (..), OpType (..))

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
    where noDigitsAtTheStart = some' (ofList symbolsOfAlphabet)
          whateverLetter = \x -> do
                rest <- many' (ofList (symbolsOfAlphabet ++ digitsOfAlphabet))
                return $ x ++ rest
          ticksAtTheEnd = \x -> do
                ticks <- many' (ofList suffixCharsOfAlphabet)
                return $ x ++ ticks
          
-- Парсер чисел
parseNum :: Parser String String Int
parseNum = parseNatural `alt` (symbol '-' *> map' negate parseNum)

parseNatural :: Parser String String Int
parseNatural =
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

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = return' Plus
toOperator '*' = return' Mult
toOperator '-' = return' Minus
toOperator '/' = return' Div
toOperator '^' = return' Pow
toOperator '!' = return' Not
toOperator _   = fail' "Failed toOperator"

toOp :: String -> Parser String String Operator
toOp "==" = return' Equal
toOp "/=" = return' Nequal
toOp "<=" = return' Le
toOp "<"  = return' Lt
toOp ">=" = return' Ge 
toOp ">"  = return' Gt
toOp "&&" = return' And
toOp "||" = return' Or
toOp _     = fail' "Failed toOp"

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
              , (eq `alt` neq `alt` leq `alt` lt `alt` geq `alt` gt, Binary NoAssoc)
              , (sum' `alt` minus, Binary LeftAssoc)
              , (mult `alt` div', Binary LeftAssoc)
              , (minus, Unary)
              , (pow, Binary RightAssoc)
              ] 
              (map' Num parseNatural `alt` map' Ident parseIdent `alt` (symbol '(' *> parseExpr <* symbol ')') )
              BinOp
              UnaryOp

compute :: AST -> Int
compute (Num x) = x
compute (BinOp Plus x y) = compute x + compute y
compute (BinOp Mult x y) = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y) = compute x `div` compute y
compute (UnaryOp Minus x) = 0 - (compute x)
compute _ = error "compute undefined"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _ -> Nothing

