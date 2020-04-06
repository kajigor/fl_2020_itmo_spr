module LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         hiding (space)
import           Combinators         (space)
import           Control.Applicative (many, some, (<|>))
import           Control.Monad       (guard)
import           Expr                (parseExpr, parseIdent)
import qualified Keyword             as K

type Expr = AST

type Var = String

data LAst
  = If
      { cond :: Expr
      , thn  :: LAst
      , els  :: LAst
      }
  | While
      { cond :: Expr
      , body :: LAst
      }
  | Assign
      { var  :: Var
      , expr :: Expr
      }
  | Read
      { var :: Var
      }
  | Write
      { expr :: Expr
      }
  | Seq
      { statements :: [LAst]
      }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If
        (BinOp Gt (Ident "X") (Num 13))
        (Write (Ident "X"))
        (While (BinOp Lt (Ident "X") (Num 42)) (Seq [Assign "X" (BinOp Mult (Ident "X") (Num 7)), Write (Ident "X")]))
    ]

kws = ["let", "if", "while", "read", "else", "write"]

keyword = K.keyword kws

body' = many separator *> symbol '{' *> many separator *> parseL <* many separator <* symbol '}' <* many separator

condition' = some space *> parseExpr <* many space

expression' = many separator *> parseExpr <* many separator

identifier' = many separator *> ident <* many separator
  where
    ident = do
      name <- parseIdent
      case runParser keyword name of
        Success _ _ -> fail' "Invalid identifier name"
        Failure _   -> return name

semicolon = string ";"

if' = If <$> (many separator *> string "if" *> condition') <*> body' <*> (string "else" *> body')

while' = While <$> (many separator *> string "while" *> condition') <*> body'

x = many separator *> string "while" *> condition'

read' = Read <$> (many separator *> string "read" *> identifier')

write' = Write <$> (many separator *> string "write" *> expression')

assign' = Assign <$> (many separator *> string "let" *> separator *> identifier' <* string "=") <*> expression'

statement' = if' <|> while' <|> (read' <* semicolon) <|> (assign' <* semicolon) <|> (write' <* semicolon)

sequence' = Seq <$> many (many separator *> statement' <* many separator)

parseL :: Parser String String LAst
parseL = sequence' <|> (Seq [] <$ many separator)
