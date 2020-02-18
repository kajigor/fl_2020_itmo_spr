module Arith where

import Control.Arrow
import Control.Applicative
import Text.Printf (printf)
import Data.Char (isDigit, digitToInt)
import qualified Sum (parseNum, splitOn)

-- "1+2*3+4*2" -> 15
data Operator = Plus
              | Mult
              | Minus
              | Div
              deriving (Eq)

data AST = BinOp Operator AST AST
         | Num  Int
         deriving (Eq)

type Parser a = String -> Maybe (a, String)

andP :: Parser a -> Parser b -> Parser (a, b)
andP f g input = do
  (a, input')  <- f input
  (b, input'') <- g input'
  pure ((a, b), input'')

parseChar :: Char -> Parser Char
parseChar _ "" = Nothing
parseChar c (x:xs)
  | x == c = Just (x, xs)
  | otherwise = Nothing

parseDiv :: Parser AST
parseDiv =
  parseOperator '/' Div (fmap (first Num) . parseNum)

parseMult :: Parser AST
parseMult =
  parseOperator '*' Mult parseDiv

parseSum :: Parser AST
parseSum =
  parseOperator '+' Plus parseMult

parseMinus :: Parser AST
parseMinus =
  parseOperator '-' Minus parseSum

parseOperator :: Char -> Operator -> Parser AST -> Parser AST
parseOperator char op child =
  liftA2 (<|>) more child
    where
      more :: Parser AST
      more =
        fmap (first $ uncurry $ BinOp op) .
        andP child (
          fmap (first snd) .
          andP (parseChar char) (parseOperator char op child)
        )

parseOpTable :: Parser AST -> [(Char, Operator)] -> Parser AST
parseOpTable =
  foldr (uncurry parseOperator)

parseExpr :: Parser AST
parseExpr =
  -- Или просто 'parseMinus'
  parseOpTable
  (fmap (first Num) . parseNum)
  [ ('-', Minus)
  , ('+', Plus)
  , ('*', Mult)
  , ('/', Div)
  ]

parseNum :: String -> Maybe (Int, String)
parseNum input =
    let (acc, rest) = span isDigit input in
    if null acc
    then Nothing
    else Just (Sum.parseNum acc, rest)

parseOp :: String -> Maybe (Operator, String)
parseOp ('+' : input) = Just (Plus, input)
parseOp ('*' : input) = Just (Mult, input)
parseOp ('/' : input) = Just (Div, input)
parseOp ('-' : input) = Just (Minus, input)
parseOp _ = Nothing

evaluate :: String -> Maybe Int
evaluate input = do
    (ast, "") <- parseExpr input
    return $ compute ast

-- parseExpr :: String -> Maybe (AST, String)
-- parseExpr input = do
--     (x, input') <- parseNum input
--     if null input'
--     then
--       return (Num x, input')
--     else do
--       (op, input'') <- parseOp input'
--       (y, input''') <- parseExpr input''
--       return (BinOp op (Num x) y, input''')

compute :: AST -> Int
compute (Num x) = x
compute (BinOp Plus x y) = compute x + compute y
compute (BinOp Mult x y) = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y) = compute x `div` compute y

instance Show Operator where
  show Plus = "+"
  show Mult = "*"
  show Minus = "-"
  show Div = "/"

instance Show AST where
  show  = printf "\n%s" . go 0
    where
      go n t =
        (if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id) $
        case t of
          BinOp op l r -> printf "%s\n%s\n%s" (show op) (go (ident n) l) (go (ident n) r)
          Num i -> show i
      ident = (+1)
