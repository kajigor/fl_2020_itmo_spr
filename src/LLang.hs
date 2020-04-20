module LLang where

import           AST                 (AST (..), Operator (..), Subst)
import           Combinators
import           Control.Applicative (many, some, (<|>))
import           Control.Monad       (guard)
import           Data.Foldable       (asum)
import           Data.List           (intercalate)
import qualified Data.Map            as M
import           Expr                (evalExpr, parseExpr, toBool)
import qualified Keyword             as K
import           Text.Printf         (printf)

type Expr = AST

type Var = String

data Configuration =
  Conf
    { subst  :: Subst
    , input  :: [Int]
    , output :: [Int]
    }
  deriving (Show, Eq)

data Program =
  Program
    { functions :: [Function]
    , main      :: LAst
    }
  deriving (Eq)

data Function =
  Function
    { name    :: String
    , args    :: [Var]
    , funBody :: LAst
    }
  deriving (Eq)

data LAst
  = If
      { cond :: Expr
      , thn  :: LAst
      , els  :: LAst
      }
  | While
      { cond :: AST
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
  | Return
      { expr :: Expr
      }
  deriving (Eq, Show)

parseDef :: Parser String String Function
parseDef = Function <$> (string "func" *> separator *> identifier') <*> args <*> body'
  where
    args = symbol '(' *> spaced (sepBy comma identifier') <* symbol ')'
    comma = many separator *> string "," <* many separator

parseProg :: Parser String String Program
parseProg = Program <$> functions' <*> parseL
  where
    functions' = many $ spaced parseDef

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If
        (BinOp Gt (Ident "X") (Num 13))
        (Write (Ident "X"))
        (While (BinOp Lt (Ident "X") (Num 42)) (Seq [Assign "X" (BinOp Mult (Ident "X") (Num 7)), Write (Ident "X")]))
    ]

kws = ["func", "if", "while", "read", "else", "write"]

keyword = K.keyword kws

body' = many separator *> symbol '{' *> many separator *> parseL <* many separator <* symbol '}' <* many separator

condition' = some space *> parseExpr <* many space

expression' = many separator *> parseExpr <* many separator

identifier' = many separator *> ident <* many separator
  where
    ident = do
      name <- parseIdent
      case runParser keyword name of
        Success {} -> fail' "Invalid identifier name"
        Failure _  -> return name

semicolon = string ";"

if' = If <$> (many separator *> string "if" *> condition') <*> body' <*> (string "else" *> body')

while' = While <$> (many separator *> string "while" *> condition') <*> body'

read' = Read <$> (many separator *> string "read" *> identifier')

write' = Write <$> (many separator *> string "write" *> expression')

assign' = Assign <$> (many separator *> identifier' <* string "=") <*> expression'

statement' = asum [if', while', read' <* semicolon, assign' <* semicolon, write' <* semicolon, return' <* semicolon]

sequence' = Seq <$> many (many separator *> statement' <* many separator)

return' = Return <$> (spaced (string "return") *> expression')

parseL :: Parser String String LAst
parseL = sequence' <|> (Seq [] <$ many separator)

initialConf :: [Int] -> Configuration
initialConf input = Conf M.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval (If cond thn els) conf@(Conf subst _ _) = do
  cond <- toBool <$> evalExpr subst cond
  if cond
    then eval thn conf
    else eval els conf
eval whl@(While cond body) conf@(Conf subst _ _) = do
  cond <- toBool <$> evalExpr subst cond
  if cond
    then eval body conf >>= eval whl
    else return conf
eval (Assign ident expr) (Conf subst input output) = do
  expr <- evalExpr subst expr
  return $ Conf (M.insert ident expr subst) input output
eval (Read ident) (Conf subst (x:xs) output) = return $ Conf (M.insert ident x subst) xs output
eval (Read ident) (Conf subst [] output) = Nothing
eval (Write expr) (Conf subst input output) = do
  expr <- evalExpr subst expr
  return $ Conf subst input (expr : output)
eval (Seq (x:xs)) conf = do
  conf'@(Conf subst' input' output') <- eval x conf
  (Conf subst'' input'' output'') <- eval (Seq xs) conf'
  return $ Conf (M.union subst'' subst') input'' output''
eval (Seq []) conf = return conf

execute :: String -> [Int] -> Maybe Configuration
execute str inp = do
  let result = runParser parseL str
  let config = initialConf inp
  case result of
    Success inp err res -> eval res config
    Failure e           -> Nothing

instance Show Function where
  show (Function name args funBody) =
    printf "%s(%s) =\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody)

instance Show Program where
  show (Program defs main) = printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

--instance Show LAst where
--  show = go 0
--    where
--      go n t =
--        let makeIdent = identation n
--         in case t of
--              If cond thn els ->
--                makeIdent $
--                printf
--                  "if %s\n%sthen\n%s\n%selse\n%s"
--                  (flatShowExpr cond)
--                  (makeIdent "")
--                  (go (ident n) thn)
--                  (makeIdent "")
--                  (go (ident n) els)
--              While cond body ->
--                makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
--              Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
--              Read var -> makeIdent $ printf "read %s" var
--              Write expr -> makeIdent $ printf "write %s" (flatShowExpr expr)
--              Seq stmts -> intercalate "\n" $ map (go n) stmts
--              Return expr -> makeIdent $ printf "return %s" (flatShowExpr expr)
--      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
--      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
--      flatShowExpr (Ident x) = x
--      flatShowExpr (Num n) = show n
--      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)

ident = (+ 1)

identation n =
  if n > 0
    then printf "%s|_%s" (concat $ replicate (n - 1) "| ")
    else id
