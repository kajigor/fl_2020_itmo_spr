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
parseExpr = parseSum

parseMult :: String -> Maybe (AST, String) 
parseMult input = do
  (num1, left) <- parseNum input
  let opr1 = Num num1
  case left of
    "" -> return (opr1, "")
    _  -> do
     (op, without_op) <- parseOp left
     (opr2, left2) <- parseMult without_op
     case op of
       Mult -> return (BinOp Mult opr1 opr2, left2)
       Div  -> return (BinOp Div opr1 opr2, left2)
       _    -> return (opr1, left)

parseSum :: String -> Maybe (AST, String) 
parseSum input = do
  (opr1, left) <- parseMult input
  case left of
    "" -> return (opr1, "")
    _  -> do
     (op, without_op) <- parseOp left
     (opr2, left2) <- parseSum without_op
     case op of
       Plus  -> return (BinOp Plus opr1 opr2, left2)
       Minus -> return (BinOp Minus opr1 opr2, left2)
       _     -> return (opr1, left)


parseNum :: String -> Maybe (Int, String) 
parseNum input =  
    let (acc, rest) = span isDigit input in 
    if null acc 
    then Nothing 
    else Just (Sum.parseNum acc, rest)

parseOp :: String -> Maybe (Operator, String)
parseOp ('+' : input) = Just (Plus, input)
parseOp ('-' : input) = Just (Minus, input)
parseOp ('/' : input) = Just (Div, input)
parseOp ('*' : input) = Just (Mult, input)
parseOp _ = Nothing

evaluate :: String -> Maybe Int
evaluate input = do 
    (ast, rest) <- parseExpr input 
    return $ compute ast

compute :: AST -> Int 
compute (Num x) = x 
compute (BinOp Plus x y) = compute x + compute y 
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Mult x y) = compute x * compute y
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
