module Expr where

import qualified Data.Map as Map
import           AST         (AST (..), Operator (..), Subst(..))
import           Combinators (Parser (..), Result (..),
                              elem', satisfy, symbol, fail')
import           Data.Char   (isDigit, digitToInt, isLetter)
import           UberExpr
import           Keyword     (keywordWeak)
import           Control.Applicative (Alternative (..))

evalExpr :: Subst -> AST -> Maybe Int
evalExpr sub (Num i) = Just i
evalExpr sub (Ident var) = Map.lookup var sub
evalExpr sub (UnaryOp op term) = applyUnOp op <*> evalExpr sub term
evalExpr sub (BinOp op l r) = applyBinOp op <*> evalExpr sub l <*> evalExpr sub r


applyUnOp :: Operator -> Maybe (Int -> Int)
applyUnOp op = case op of
  Minus -> return negate
  Not   -> return (\x -> if x == 0 then 1 else 0)
  _     -> Nothing

numToBool :: (Num a, Eq a) => a -> Bool
numToBool = (/=) 0

boolToNum :: Num a => Bool -> a
boolToNum b = if b then 1 else 0

applyBinOp :: Operator -> Maybe (Int -> Int -> Int)
applyBinOp op = let
  asIntOp :: (a -> a -> Bool) -> (Int -> a) -> (Int -> Int -> Int)
  asIntOp opfunc conv l r = boolToNum $ opfunc (conv l) (conv r)
  in case op of
    Plus   -> return (+)
    Mult   -> return (*)
    Minus  -> return (-)
    Div    -> return div
    Pow    -> return (^)
    Equal  -> return $ asIntOp (==) id
    Nequal -> return $ asIntOp (/=) id
    Gt     -> return $ asIntOp (>) id
    Ge     -> return $ asIntOp (>=) id
    Lt     -> return $ asIntOp (<) id
    Le     -> return $ asIntOp (<=) id
    And    -> return $ asIntOp (&&) numToBool
    Or     -> return $ asIntOp (||) numToBool
    _      -> Nothing

parseExpr :: Parser String String AST
parseExpr = uberExpr 
              [(makeOpParser ["||"], Binary RightAssoc),
               (makeOpParser ["&&"], Binary RightAssoc),
               (makeOpParser ["!"], Unary),
               (makeOpParser ["==", "/=", "<=", "<", ">=", ">"], Binary NoAssoc),
               (makeOpParser ["+", "-"], Binary LeftAssoc),
               (makeOpParser ["*", "/"], Binary LeftAssoc),
               (makeOpParser ["-"], Unary),
               (makeOpParser ["^"], Binary RightAssoc)]
              (parseTerm <|> Ident <$> parseIdent)
              BinOp
              UnaryOp

-- Парсер операторов на основе keyword парсера
-- Допускает произвольное число пробелов вокруг операторов
makeOpParser :: [String] -> Parser String String Operator
makeOpParser ops = keywordWeak ops >>= asOperator where
  asOperator :: String -> Parser String String Operator
  asOperator op =
    case op of
      "+"  -> return Plus
      "*"  -> return Mult
      "-"  -> return Minus
      "/"  -> return Div
      "^"  -> return Pow
      "==" -> return Equal
      "/=" -> return Nequal
      ">"  -> return Gt
      ">=" -> return Ge
      "<"  -> return Lt
      "<=" -> return Le
      "!"  -> return Not
      "&&" -> return And
      "||" -> return Or
      _    -> fail' "Failed to parse an operator"
      

parseIdent :: Parser String String String
parseIdent = (:) <$> letter <*> many (letter <|> digit) where
  underscore = symbol '_'
  letter = satisfy isLetter <|> underscore
  digit = satisfy isDigit

parseNatural :: Parser String String Int
parseNatural = toNum <$> some digit where
  toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
  digit = satisfy isDigit

parseNum :: Parser String String Int
parseNum = parseNatural <|> (negate <$ sign <*> (spaces *> parseNatural)) where
  sign = symbol '-'
  spaces = many $ symbol ' '

parseTerm :: Parser String String AST
parseTerm = Num <$> parseNatural <|> (lbr *> spaces *> parseExpr <* spaces <* rbr)
  where
    lbr = symbol '('
    rbr = symbol ')'
    spaces = many $ symbol ' '

-- Парсер для операторов
-- Сейчас не используется, оставил, чтобы не падали
-- более ранние тесты, которые его напрямую вызывают
-- Позже удалю
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

toOperator :: Char -> Parser String String Operator
toOperator '+' = return Plus
toOperator '*' = return Mult
toOperator '-' = return Minus
toOperator '/' = return Div
toOperator _   = fail' "Failed toOperator"

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
compute _                 = error "compute undefined"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest _ ast | null (stream rest) -> return $ compute ast
    _                                       -> Nothing
