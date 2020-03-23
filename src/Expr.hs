module Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), elem', int, nat,
                                      satisfy, sepBy1', symbol)
import           Control.Applicative
import           Data.Char           (digitToInt, isAlphaNum, isDigit, isLetter)
import           Data.Foldable       (asum)
import           Debug.Trace
import           UberExpr            (Associativity (..), uberExpr)

-- Парсер для произведения/деления термов
-- Парсер чисел
parseNum :: Parser String String Int
parseNum = int

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator
  -- Преобразование символов операторов в операторы
  where
    toOperator :: Char -> Parser String String Operator
    toOperator '+' = return Plus
    toOperator '*' = return Mult
    toOperator '-' = return Minus
    toOperator '/' = return Div
    toOperator _   = fail' "Failed toOperator"

fail' :: e -> Parser e i a
fail' e = Parser $ \input -> Failure e

parseIdent :: Parser String String String
parseIdent = (++) <$> some (char <|> control) <*> many (alphaNum <|> control)
  where
    char = satisfy isLetter
    digit = satisfy isDigit
    control = satisfy (== '_')
    alphaNum = satisfy isAlphaNum

-- Парсер арифметических выражений над целыми числами с операциями +,-,*,/.
parseExpr :: Parser String String AST
parseExpr = uberExpr table ast builder
  where
    table :: [(Parser String String Operator, Associativity)]
    table =
      [ (Or <$ mapM symbol "||", RightAssoc)
      , (And <$ mapM symbol "&&", RightAssoc)
      , ( asum
            [ Ge <$ mapM symbol ">="
            , Le <$ mapM symbol "<="
            , Lt <$ mapM symbol "<"
            , Gt <$ mapM symbol ">"
            , Nequal <$ mapM symbol "/="
            , Equal <$ mapM symbol "=="
            ]
        , NoAssoc)
      , (asum [Plus <$ mapM symbol "+", Minus <$ mapM symbol "-"], LeftAssoc)
      , (asum [Mult <$ mapM symbol "*", Div <$ mapM symbol "/"], LeftAssoc)
      , (Pow <$ mapM symbol "^", RightAssoc)
      ]
    ast = number <|> identifier <|> (parseLeftBracket *> parseExpr <* parseRightBracket)
    number = Num <$> int
    identifier = Ident <$> parseIdent
    parseLeftBracket = symbol '('
    parseRightBracket = symbol ')'
    builder = BinOp

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y

evaluate :: String -> Maybe Int
evaluate input =
  case runParser parseExpr input of
    Success rest ast
      | null rest -> return $ compute ast
    _ -> Nothing
