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
         (num, rest) <- parseNum input
         case rest of 
             "" -> return $ (Num num, "")
             _ -> do
                 (operator, resttail) <- parseOp rest
                 case operator of
                         Mult -> do
                             (num2, rest2) <- parseMult resttail
                             return $ (BinOp Mult (Num num) num2, rest2)
                         Div -> do
                             (num2, rest2) <- parseMult resttail
                             return $ (BinOp Div (Num num) num2, rest2)
                         _ -> return $ (Num num, rest)

                   
parseSum :: String -> Maybe (AST, String) 
parseSum input = do 
         (mul, rest) <- parseMult input
         case rest of
             "" -> return $ (mul, "")
             _ -> do 
                 (operator, resttail) <- parseOp rest
                 case operator of
                         Plus -> do
                             (mul2, rest2) <- parseSum resttail
                             return $ (BinOp Plus mul mul2, rest2)
                         Minus -> do
                             (mul2, rest2) <- parseSum resttail
                             return $ (BinOp Minus mul mul2, rest2)
                         _ -> return $ (mul, rest)


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

--parseExpr :: String -> Maybe (AST, String) 
--parseExpr input = do 
--    (x, input') <- parseNum input 
--    if null input' 
--    then 
--      return (Num x, input')
--    else do 
--      (op, input'') <- parseOp input' 
--      (y, input''') <- parseExpr input''
--      return (BinOp op (Num x) y, input''')

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
