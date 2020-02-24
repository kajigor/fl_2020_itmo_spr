module Arith where 

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

parseExpr :: String -> Maybe (AST, String) 
parseExpr = parseSumSub

parseMult :: String -> Maybe (AST, String) 
parseMult input = do 
    (x, input') <- parseNum input 
    if null input' 
    then 
      return (Num x, input')
    else do 
      (op, input'') <- parseOp input'
      if op == Mult
      then do
      	(y, input''') <- parseMult input''
      	return (BinOp op (Num x) y, input''')
      else return (Num x, input')


parseSum :: String -> Maybe (AST, String) 
parseSum input = do 
    (x, input') <- parseMult input 
    if null input' 
    then 
      return (x, input')
    else do 
      (op, input'') <- parseOp input'
      if op == Plus
      then do
      	(y, input''') <- parseSum input''
      	return (BinOp op x y, input''')
      else return (x, input')

parseMultDiv :: String -> Maybe (AST, String) 
parseMultDiv input = do 
    (x, input') <- parseNum input 
    if null input' 
    then 
      return (Num x, input')
    else do 
      (op, input'') <- parseOp input'
      if op == Mult || op == Div
      then do
      	(y, input''') <- parseMultDiv input''
      	return (BinOp op (Num x) y, input''')
      else return (Num x, input')


parseSumSub :: String -> Maybe (AST, String) 
parseSumSub input = do 
    (x, input') <- parseMultDiv input 
    if null input' 
    then 
      return (x, input')
    else do 
      (op, input'') <- parseOp input'
      if op == Plus || op == Minus
      then do
      	(y, input''') <- parseSumSub input''
      	return (BinOp op x y, input''')
      else return (x, input')


parseNum :: String -> Maybe (Int, String) 
parseNum input =  
    let (acc, rest) = span isDigit input in 
    if null acc 
    then Nothing 
    else Just (Sum.parseNum acc, rest)

parseOp :: String -> Maybe (Operator, String)
parseOp ('+' : input) = Just (Plus, input)
parseOp ('*' : input) = Just (Mult, input)
parseOp ('-' : input) = Just (Minus, input)
parseOp ('/' : input) = Just (Div, input)
parseOp _ = Nothing 

evaluate :: String -> Maybe Int
evaluate input = do 
    (ast, rest) <- parseExpr input 
    return $ compute ast 

{- parseExpr :: String -> Maybe (AST, String) 
parseExpr input = do 
    (x, input') <- parseNum input 
    if null input' 
    then 
      return (Num x, input')
    else do 
      (op, input'') <- parseOp input' 
      (y, input''') <- parseExpr input''
      return (BinOp op (Num x) y, input''') -}

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
