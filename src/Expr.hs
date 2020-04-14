module Expr where

import           AST        
import           Combinators 
import           Data.Char   
import UberExpr
import Control.Applicative (Alternative (..))  
import Keyword
import Ops
import Data.Either
import Data.Bifunctor
import Data.Maybe
import qualified Data.Map    as Map

operators = ["+", "-", "*", "/", "^", "==", "/=", ">", "<", ">=", "<=", "&&", "||", "!"]

sum'  = symbols "+" >>= toOperator
minus = symbols "-" >>= toOperator
mult = symbols "*" >>= toOperator
neq = symbols "/=" >>= toOperator
div'  = symbols "/" >>= toOperator
pow  = symbols "^" >>= toOperator
eq  = symbols "==" >>= toOperator
ge = symbols ">=" >>= toOperator
le = symbols "<=" >>= toOperator
gt = symbols ">" >>= toOperator
lt = symbols "<" >>= toOperator
and' = symbols "&&" >>= toOperator
or' = symbols "||" >>= toOperator
not'= symbols "!" >>= toOperator


parseExpr :: Parser String String AST

parseExpr = uberExpr [(or', Binary RightAssoc),
                    (and',Binary RightAssoc),
                    (not', Unary),
                    (eq <|> neq <|>  ge <|> le <|> gt <|> lt,Binary NoAssoc),
                    (sum' <|> minus,Binary LeftAssoc),
                    (mult <|> div',Binary LeftAssoc),
                    (minus, Unary),
                    (pow,Binary RightAssoc)]
                    parseTerm
                    BinOp
                    UnaryOp


-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = uberExpr [(mult <|> div',Binary LeftAssoc)] parseTerm BinOp UnaryOp

-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = uberExpr [ (sum' <|> minus,Binary LeftAssoc), (mult <|> div',Binary LeftAssoc)
                    ]
                    parseTerm
                    BinOp
                    UnaryOp

parseIdent :: Parser String String String
parseIdent = do
  c <- satisfy isLetter <|> symbol '_'
  l <- many (satisfy isLetter <|> satisfy isDigit <|> symbol '_')
  return $ c:l

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = op operators >>= toOperator


-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+" = return Plus
toOperator "*" = return Mult
toOperator "-" = return Minus
toOperator "/" = return Div
toOperator "^" = return Pow
toOperator "==" = return Equal
toOperator "/=" = return Nequal
toOperator ">" = return Gt
toOperator ">=" = return Ge
toOperator "<" = return Lt
toOperator "<=" = return Le
toOperator "&&" = return And
toOperator "||" = return Or
toOperator "!" = return Not

toOperator _   = fail' "Failed toOperator"

-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTerm :: Parser String String AST
parseTerm =
    fmap Num parseNum <|>
    (lbr *> spaceis *> parseExpr <* spaceis <* rbr
    ) <|> (spaceis *> fmap Ident parseIdent <* spaceis) 
  where
    lbr = spaceis *> symbol '('
    rbr = symbol ')'

-- Парсер арифметических выражений над целыми числами с операциями +,-,*,/.

parseNum = res where
  res = do
    l <- parseNumStr
    return $ foldl (\acc a -> acc * 10 + (digitToInt a)) 0 l

  parseNumStr = do
        digit <- satisfy isDigit
        number <- parseNumStr <|> (return "")
        return $ digit:number

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

evalExpr conf (BinOp Equal x' y') = (do
  x <- evalBool conf x'
  y <- evalBool conf y'
  return $ Left (x == y)) <|> (do
        x <- evalNum conf x'
        y <- evalNum conf y'
        return $ Left (x == y)
        )
evalExpr conf (BinOp Nequal x' y') = fmap (bimap not id) (evalExpr conf (BinOp Equal x' y'))
evalExpr conf expr = (do
  res <- evalBool conf expr
  return $ Left res) <|> (do
    res <- evalNum conf expr
    return $ Right res)

evalBool conf (BinOp op x' y') 
  | Just x <- evalNum conf x', 
    Just y <- evalNum conf y' = case op of
      Gt -> return $ x > y
      Ge -> return $ x >= y
      Lt -> return $ x < y
      Le -> return $ x <= y
      _  -> Nothing

evalBool conf (BinOp op x' y')
  | Just x <- evalBool conf x',
    Just y <- evalBool conf y' = case op of
      And -> return $ x && y
      Or -> return $ x || y
      _ -> Nothing
      
evalBool conf (UnaryOp Not x' ) = do
  x <- evalBool conf x'
  return $ (not x)
evalBool _ _ = Nothing

evalNum conf (Num x) = Just x
evalNum conf (Ident x) = Map.lookup x conf
evalNum conf (BinOp op x' y') = do
  x <- evalNum conf x'
  y <- evalNum conf y'
  return $ case op of
    Plus -> x + y
    Mult -> x * y
    Minus -> x - y
    Div -> x `div` y
    Pow -> x ^ y
evalNum conf (UnaryOp Minus x') = do
  x <- evalNum conf x'
  return (-x)
evalNum _ _ = Nothing