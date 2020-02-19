module Arith where

import           Data.Char   (digitToInt, isDigit)
import qualified Sum         (parseNum, splitOn)
import           Text.Printf (printf)

-- "1+2*3+4*2" -> 15
data Operator
  = Plus
  | Mult
  | Minus
  | Div
  deriving (Eq)

data AST
  = BinOp Operator AST AST
  | Num Int
  deriving (Eq)

parseExpr :: String -> Maybe (AST, String)
parseExpr = parseSum

parseMult :: String -> Maybe (AST, String)
parseMult input = do
  (number, rest) <- parseNum input
  let lhs = Num number
  if null rest
    then Just (Num number, rest)
    else do
      (operator, rest') <- parseOp rest
      if operator == Mult || operator == Div
        then do
          (tree, rest'') <- parseMult rest'
          Just (BinOp operator lhs tree, rest'')
        else Just (lhs, rest)

parseSum :: String -> Maybe (AST, String)
parseSum input = do
  (lhs, rest) <- parseMult input
  if null rest
    then Just (lhs, rest)
    else do
      (operator, rest') <- parseOp rest
      if operator == Minus || operator == Plus
        then do
          (tree, rest'') <- parseSum rest'
          Just (BinOp operator lhs tree, rest'')
        else Just (lhs, rest)

parseNum :: String -> Maybe (Int, String)
parseNum input =
  let (acc, rest) = span isDigit input
   in if null acc
        then Nothing
        else Just (Sum.parseNum acc, rest)

parseOp :: String -> Maybe (Operator, String)
parseOp ('+':input) = Just (Plus, input)
parseOp ('*':input) = Just (Mult, input)
parseOp ('/':input) = Just (Div, input)
parseOp ('-':input) = Just (Minus, input)
parseOp _           = Nothing

evaluate :: String -> Maybe Int
evaluate input = do
  (ast, rest) <- parseExpr input
  return $ compute ast

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Div x y)   = compute x `div` compute y

instance Show Operator where
  show Plus  = "+"
  show Mult  = "*"
  show Minus = "-"
  show Div   = "/"

instance Show AST where
  show = printf "\n%s" . go 0
    where
      go n t =
        (if n > 0
           then printf "%s|_%s" (concat $ replicate (n - 1) "| ")
           else id) $
        case t of
          BinOp op l r -> printf "%s\n%s\n%s" (show op) (go (ident n) l) (go (ident n) r)
          Num i -> show i
      ident = (+ 1)
