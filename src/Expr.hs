module Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), elem', int, nat,
                                      satisfy, sepBy1', space, symbol)
import           Control.Applicative
import           Data.Char           (digitToInt, isAlphaNum, isDigit, isLetter)
import           Data.Foldable       (asum)
import           Debug.Trace
import           UberExpr            (Associativity (..), OpType (..), uberExpr)

evalExpr :: Subst -> AST -> Maybe Int
evalExpr = error "evalExpr undefined"
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
spaces = many space

spaced p = spaces *> p <* spaces

parseExpr :: Parser String String AST
parseExpr = uberExpr table ast binary unary
  where
    table :: [(Parser String String Operator, OpType)]
    table =
      [ (Or <$ spaced (mapM symbol "||"), Binary RightAssoc)
      , (And <$ spaced (mapM symbol "&&"), Binary RightAssoc)
      , (Not <$ spaced (mapM symbol "!"), Unary)
      , ( asum
            [ Ge <$ spaced (mapM symbol ">=")
            , Le <$ spaced (mapM symbol "<=")
            , Lt <$ spaced (mapM symbol "<")
            , Gt <$ spaced (mapM symbol ">")
            , Nequal <$ spaced (mapM symbol "/=")
            , Equal <$ spaced (mapM symbol "==")
            ]
        , Binary NoAssoc)
      , (asum [Plus <$ spaced (mapM symbol "+"), Minus <$ spaced (mapM symbol "-")], Binary LeftAssoc)
      , (asum [Mult <$ spaced (mapM symbol "*"), Div <$ spaced (mapM symbol "/")], Binary LeftAssoc)
      , (Minus <$ spaced (mapM symbol "-"), Unary)
      , (Pow <$ spaced (mapM symbol "^"), Binary RightAssoc)
      ]
    ast = number <|> identifier <|> (parseLeftBracket *> parseExpr <* parseRightBracket)
    number = Num <$> spaced nat
    identifier = Ident <$> spaced parseIdent
    parseLeftBracket = symbol '('
    parseRightBracket = symbol ')'
    binary = BinOp
    unary = UnaryOp

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
