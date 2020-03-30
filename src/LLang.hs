{-# LANGUAGE ScopedTypeVariables #-}
module LLang where

import Prelude hiding (seq, read)
import AST
import Combinators
import Expr

import Data.Char (isSpace)
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

parseL :: Parser String String LAst
parseL = seq

seq :: Parser String String LAst =
  Seq <$> sepBy1 spc (symbol '(' *> spc *> stmt <* spc <* symbol ')')

stmt = if' <|> while <|> assign <|> read <|> write <|> seq

if' =
  If <$>
  (
    string "if" *> spc *> symbol '(' *> spc *> parseExpr <* spc <* symbol ')'
  ) <*>
  (
    spc *>

    symbol '{' *> spc *> stmt <* spc <* symbol '}'

    <* spc
  ) <*>
  (
    spc *>

    symbol '{' *> spc *> stmt <* spc <* symbol '}'
  )

while =
  While <$>
  (
    string "while" *> spc *> symbol '(' *> spc *> parseExpr <* spc <* symbol ')'
  ) <*>
  (
    spc *>
    symbol '{' *> spc *> seq <* spc <* symbol '}'
  )

assign = Assign <$> ident <*> (space *> string ":=" *> spc *> parseExpr)
read = Read <$> (string "read" *> space *> ident)
write = Write <$> (string "write" *> space *> parseExpr)
spc = many (satisfy isSpace)
space = some (satisfy isSpace)
ident = (:) <$> firstLetter <*> rest
  where
    firstLetter = lowerCase <|> upperCase <|> symbol '_'
    rest = many $ (firstLetter <|> number)

    lowerCase = between 'a' 'z'
    upperCase = between 'A' 'Z'
    number = between '0' '9'
    between f t = satisfy (\x -> x >= f && x <= t)
