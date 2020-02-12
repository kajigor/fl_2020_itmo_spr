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
  (x, rest) <- parseNum input
  res <- if null rest
    then Just (Num x, "")  
    else
     case (parseOp rest) of
        Just (Mult, rest') -> fmap (\(ast, rest) -> (BinOp Mult (Num x) ast, rest)) (parseMult rest') 
        Just (Div, rest') -> fmap (\(ast, rest) -> (BinOp Div (Num x) ast, rest)) (parseMult rest')
        _ -> Just (Num x, rest)
  return res

parseSum :: String -> Maybe (AST, String) 
parseSum input = do
  (x, rest) <- parseMult input
  res <- if null rest
    then Just  (x, "")  
    else
     case (parseOp rest) of
        Just (Plus, rest') -> fmap (\(ast, rest) -> (BinOp Plus x ast, rest)) (parseSum rest')
        Just (Minus, rest') -> fmap (\(ast, rest) -> (BinOp Minus x ast, rest)) (parseSum rest') 
        _ -> Just (x, rest)
  return res


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
compute (BinOp Div x y) = compute x `div` compute y 
compute (BinOp Minus x y) = compute x - compute y 
compute _ = error "compute not implemented"

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
