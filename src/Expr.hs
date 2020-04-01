module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), alt, elem', fail', map',
                              return', satisfy, symbol, empty', many', some')
import           Data.Char   (isDigit, digitToInt)
import           UberExpr    (uberExpr, foldl1', Associativity (..))

sepByOp :: Parser String String Operator -> Parser String String AST -> Parser String String [(Maybe Operator, AST)]
sepByOp sep elem = prsr `alt` prsr'
  where prsr = do
          el <- elem
          op <- sep
          elems <- sepByOp sep elem
          return ((Just op, el):elems)
        prsr' = do
          el <- elem
          return [(Nothing, el)]

parseMult :: Parser String String AST
parseMult = do
      lst <- sepByOp parseMultDiv parseTerm
      return $ snd (foldl1' BinOp lst)

parseSum :: Parser String String AST
parseSum = do
      lst <- sepByOp parseOp parseMult
      return $ snd (foldl1' BinOp lst)


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
parseNum =
    map' toNum go `alt` (symbol '-' *> map' negate parseNum)
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
toOperator '^' = return' Pow
toOperator _   = fail' "Failed toOperator"

symbols :: String -> Parser String String String
symbols [] = return' []
symbols (x:xs) = do
    c <- symbol x
    rest <- symbols xs
    return $ x:xs

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
eq    = symbols "=="  >>= toOp
neq   = symbols "/=" >>= toOp
leq   = symbols "<="  >>= toOp
lt    = symbols "<"   >>= toOp
geq   = symbols ">="  >>= toOp
gt    = symbols ">"   >>= toOp
and'  = symbols "&&"  >>= toOp
or'   = symbols "||"  >>= toOp

-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTerm :: Parser String String AST
parseTerm = map' Num parseNum `alt` map' Ident parseIdent `alt` do
      symbol '('
      e <- parseSum
      symbol ')'
      return e

-- Парсер арифметических выражений над целыми числами с арифметическими операциями +,-,*,/,^ и бинарными логическими операциями.
parseExpr :: Parser String String AST
parseExpr =
     uberExpr [ (or', RightAssoc)
              , (and', RightAssoc)
              , (eq `alt` neq `alt` leq `alt` lt `alt` geq `alt` gt, NoAssoc)
              , (sum' `alt` minus, LeftAssoc)
              , (mult `alt` div', LeftAssoc)
              , (pow, RightAssoc)
              ] 
              (map' Num parseNum `alt` map' Ident parseIdent `alt` (symbol '(' *> parseExpr <* symbol ')') )
              BinOp

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

