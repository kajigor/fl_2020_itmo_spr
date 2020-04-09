module Expr where

import           AST         (AST (..), Operator (..), Subst (..))
import           Combinators
import           Data.Char   (isDigit, digitToInt)
import           Control.Applicative
import           Data.Functor ((<&>))
import           Data.Foldable (asum)
import           UberExpr
import           Control.Monad

evalExpr :: Subst -> AST -> Maybe Int
evalExpr = error "evalExpr undefined"

-- Парсер арифметических выражений над целыми числами
parseExpr :: Parser String String AST
parseExpr = error "parseExpr undefined"

-- Парсер для произведения/деления термов
parseMult :: Parser String String AST
parseMult = uberExpr
  [ (symbol '*' *> pure Mult, LeftAssoc)
  , (symbol '/' *> pure Div,  LeftAssoc)
  ]
  parseTerm
  BinOp

-- Парсер для сложения/вычитания множителей
parseSum :: Parser String String AST
parseSum = uberExpr
  [ (symbol '+' *> pure Plus,  LeftAssoc)
  , (symbol '-' *> pure Minus, LeftAssoc)
  ]
  parseMult
  BinOp

-- Парсер чисел
parseNum :: Parser String String Int
parseNum =
  number <|> symbol '-' *> (negate <$> number)
  where
    number = fmap toNum go
    digit = satisfy isDigit
    toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
    go = some digit

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = pure Plus
toOperator '*' = pure Mult
toOperator '-' = pure Minus
toOperator '/' = pure Div
toOperator _   = fail' "Failed toOperator"

-- Парсер для терма: либо число, либо выражение в скобках.
-- Скобки не хранятся в AST за ненадобностью.
parseTerm :: Parser String String AST
parseTerm =
    fmap Num parseNum <|>
    (do
        lbr
        e <- parseSum
        rbr
        pure e
    )
  where
    lbr = symbol '('
    rbr = symbol ')'

-- Парсер арифметических выражений над целыми числами с операциями +,-,*,/.
parseExpr :: Parser String String AST
parseExpr =
  uberExpr
  [ (mapM symbol "||" *> pure Or, RightAssoc)
  , (mapM symbol "&&" *> pure And, RightAssoc)
  , (asum [ mapM symbol "==" *> pure Equal
          , mapM symbol "/=" *> pure Nequal
          , mapM symbol "<=" *> pure Le
          , symbol          '<' *> pure Lt
          , mapM symbol ">=" *> pure Ge
          , symbol          '>' *> pure Gt
          ], NoAssoc)
  , (symbol '+' *> pure Plus <|> symbol '-' *> pure Minus, LeftAssoc)
  , (symbol '*' *> pure Mult <|> symbol '/' *> pure Div, LeftAssoc)
  , (symbol '^' *> pure Pow, RightAssoc)
  ]
  ( (Num <$> parseNum) <|>
    (Ident <$> parseIdent) <|>
    (symbol '(' *> parseExpr <* symbol ')')
  )
  BinOp

parseIdent :: Parser String String String
parseIdent = liftM2 (:) alpha (many alphaNumeric)
  where
    alpha :: Parser String String Char
    alpha = satisfy (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_')
    alphaNumeric :: Parser String String Char
    alphaNumeric = alpha <|> satisfy (\c -> c >= '0' && c <= '9')

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
