module Expr where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Combinators         (InputStream (..), Parser (..),
                                      Result (..), elem', fail', nat,
                                      parseIdent, runParser, sepBy, separator,
                                      spaced, string, symbol)
import           Control.Applicative (many, (<|>))
import           Data.Char           (digitToInt, isDigit)
import           Data.Foldable       (asum)
import qualified Data.Map            as M
import           UberExpr

evalExpr :: Subst -> AST -> Maybe Int
evalExpr subst (BinOp op lhs rhs) = binaryOp op <*> evalExpr subst lhs <*> evalExpr subst rhs
evalExpr subst (UnaryOp op v) = unaryOp op <*> evalExpr subst v
evalExpr subst (Ident ident) = M.lookup ident subst
evalExpr subst (Num n) = return n

fromBool :: Bool -> Int
fromBool = fromEnum

toBool :: Int -> Bool
toBool = (> 0)

fromBoolBin f x y = fromBool $ f x y

fromToBoolBin f x y = fromBool $ f (toBool x) (toBool y)

unaryOp :: Operator -> Maybe (Int -> Int)
unaryOp Not   = return (fromBool . not . toBool)
unaryOp Minus = return negate
unaryOp _     = Nothing

binaryOp :: Operator -> Maybe (Int -> Int -> Int)
binaryOp Plus   = return (+)
binaryOp Mult   = return (*)
binaryOp Minus  = return (-)
binaryOp Div    = return div
binaryOp Pow    = return (^)
binaryOp Equal  = return $ fromBoolBin (==)
binaryOp Nequal = return $ fromBoolBin (/=)
binaryOp Gt     = return $ fromBoolBin (>)
binaryOp Ge     = return $ fromBoolBin (>=)
binaryOp Lt     = return $ fromBoolBin (<)
binaryOp Le     = return $ fromBoolBin (<=)
binaryOp And    = return $ fromToBoolBin (&&)
binaryOp Or     = return $ fromToBoolBin (||)
binaryOp _      = Nothing

-- Парсер арифметических выражений над целыми числами
parseExpr :: Parser String String AST
parseExpr = parseExpr' parseIdent

parseExpr' ident = uberExpr table ast binary unary
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
    ast = number <|> funcCall <|> identifier <|> funcCall <|> (parseLeftBracket *> parseExpr <* parseRightBracket)
    number = Num <$> spaced nat
    identifier = Ident <$> spaced ident
    funcCall = FunctionCall <$> spaced ident <*> args
    args = parseLeftBracket *> spaced (sepBy comma parseExpr) <* parseRightBracket
    comma = many separator *> string "," <* many separator
    parseLeftBracket = symbol '('
    parseRightBracket = symbol ')'
    binary = BinOp
    unary = UnaryOp

-- Парсер чисел
parseNum :: Parser String String Int
parseNum = nat

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

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
compute _                 = error "compute undefined"

evaluate :: String -> Maybe Int
evaluate input =
  case runParser parseExpr input of
    Success rest _ ast
      | null (stream rest) -> return $ compute ast
    _ -> Nothing
