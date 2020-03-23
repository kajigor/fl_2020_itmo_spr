module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..),
                              elem', satisfy, symbol, fail')
import           Data.Char   (isDigit, digitToInt, isLetter)
import           UberExpr
import           Keyword     (keywordWeak)
import           Control.Applicative (Alternative (..))

parseExpr :: Parser String String AST
parseExpr = uberExpr 
              [(makeOpParser ["||"], RightAssoc),
               (makeOpParser ["&&"], RightAssoc),
               (makeOpParser ["==", "/=", "<=", "<", ">=", ">"], NoAssoc),
               (makeOpParser ["+", "-"], LeftAssoc),
               (makeOpParser ["*", "/"], LeftAssoc),
               (makeOpParser ["^"], RightAssoc)]
              (parseTerm <|> Ident <$> parseIdent)
              BinOp

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
      "&&" -> return And
      "||" -> return Or
      _    -> fail' "Failed to parse an operator"
      

parseIdent :: Parser String String String
parseIdent = (:) <$> letter <*> many (letter <|> digit) where
  underscore = symbol '_'
  letter = satisfy isLetter <|> underscore
  digit = satisfy isDigit

-- Парсер чисел
parseNum :: Parser String String Int
parseNum = baseNumParser <|> (signAsAction <$> sign <*> (spaces *> baseNumParser)) where
  sign = symbol '-'
  signAsAction c = case c of
                     '-' -> negate
                     _   -> error "signAsAction failed"
  spaces = many $ symbol ' '
  baseNumParser = toNum <$> some digit
  toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
  digit = satisfy isDigit

parseTerm :: Parser String String AST
parseTerm = Num <$> parseNum <|> (lbr *> spaces *> parseExpr <* spaces <* rbr)
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
