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

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)


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

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval = error "eval not defined"

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id in

        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      ident = (+1)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
