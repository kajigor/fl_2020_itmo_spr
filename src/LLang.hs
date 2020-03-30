{-# LANGUAGE ScopedTypeVariables #-}
module LLang where

import AST
import Combinators
import Expr

import Control.Monad
import Control.Applicative


type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: Expr, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

parseL :: Parser String String LAst
parseL = seq
  where
    -- seq :: Parser String String LAast
    seq :: Parser String String LAst =
      Seq <$>
      (symbol '(' *>
      string "do" *>
      many (symbol '(' *> stmt <* symbol ')') <*
      symbol ')'
      )

    stmt = if' <|> while <|> assign <|> read <|> write <|> seq

    if' =
      If <$>
      (
        string "if" *> symbol '(' *> expr <* symbol ')'
      ) <*>
      (
        symbol '{' *> stmt <* symbol '}'
      ) <*>
      (
        symbol '{' *> stmt <* symbol '}'
      )

    while =
      While <$>
      (
        string "while" *> symbol '(' *> expr <* symbol ')'
      ) <*>
      (
        symbol '{' *> stmt <* symbol '}'
      )

    assign = Assign <$> ident <*> (string ":=" *> expr)
    read = Read <$> (string "read" *> ident)
    write = Write <$> (string "write" *> spc *> expr)
    expr = parseExpr
    spc = many (symbol ' ')
    ident = (:) <$> firstLetter <*> rest
      where
        firstLetter = lowerCase <|> upperCase <|> symbol '_'
        rest = many $ (firstLetter <|> number)

        lowerCase = between 'a' 'z'
        upperCase = between 'A' 'Z'
        number = between '0' '9'
        between f t = satisfy (\x -> x >= f && x <= t)
