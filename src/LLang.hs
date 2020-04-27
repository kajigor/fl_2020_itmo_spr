module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators (Parser (..), symbol, satisfy, symbols, fail')
import Expr (parseExpr, parseIdent, evalExpr, falsy)
import qualified Data.Map as Map
import Data.List (intercalate)
import Text.Printf (printf)

import Control.Applicative

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)


data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst }

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  | Return { expr :: Expr }
  deriving (Eq)

parseWhitespace :: Parser String String String
parseWhitespace = many (symbol ' ' <|> symbol '\n' <|> symbol '\t')

skipWhitespace :: Parser String String a -> Parser String String a
skipWhitespace parser = parseWhitespace *> parser <* parseWhitespace

parseDef :: Parser String String Function
parseDef = error "parseDef undefined"

parseProg :: Parser String String Program
parseProg = undefined
    

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

inBrackets parser = skipWhitespace (symbol '(') *> skipWhitespace parser <* skipWhitespace (symbol ')')

parseStatement :: String -> Parser String String LAst
parseStatement "if" = do
        expression <- inBrackets parseExpr 
        skipWhitespace $ symbols "then"
        thenStatement <- inBrackets parseSeq 
        skipWhitespace $ symbols "else"
        elseStatement <- inBrackets parseSeq 
        return $ If expression thenStatement elseStatement

parseStatement "while" = do
        expression <- inBrackets parseExpr 
        statement <- inBrackets parseSeq
        return $ While expression statement

parseStatement "let" = do
        varname <- inBrackets parseIdent
        value <- inBrackets parseExpr
        return $ Assign varname value

parseStatement "read" = do
        varname <- inBrackets parseIdent
        return $ Read varname

parseStatement "write" = do
        expression <- inBrackets parseExpr
        return $ Write expression

parseStatement "return" = do
        expression <- inBrackets parseExpr
        return $ Return expression

parseStatement tok = fail' ("unexpected token: " ++ tok)

parseWord :: Parser String String String
parseWord = some (satisfy (/= ' '))

parseStatements :: Parser String String [LAst]
parseStatements = parser <|> return []
    where parser = do
            statement <- inBrackets (parseWord >>= parseStatement)
            stats <- parseStatements
            return $ statement:stats

parseSeq :: Parser String String LAst
parseSeq = Seq <$> parseStatements

parseL :: Parser String String LAst
parseL = parseSeq 

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration

eval (If expr thenSt elseSt) conf@(Conf env _ _) = do
        value <- evalExpr expr env
        if value /= falsy
            then eval thenSt conf
            else eval elseSt conf

eval (While expr st) conf@(Conf env _ _) = do
        value <- evalExpr expr env
        if value /= falsy
            then eval st conf
            else return conf

eval (Assign ident expr) (Conf env input output) = do
        value <- evalExpr expr env
        return $ Conf (Map.insert ident value env) input output

eval (Read ident) (Conf env [] output) = Nothing
eval (Read ident) (Conf env (x:input) output) = return $ Conf (Map.insert ident x env) input output

eval (Write expr) (Conf env input output) = do
    value <- evalExpr expr env
    return $ Conf env input (value:output)

-- Не полностью уверен, что это корректное поведение.
eval (Return expr) env = eval (Write expr) env

--eval (Seq [x]) env = eval x env
eval (Seq []) env = return env
eval (Seq (x:xs)) env = eval x env >>= eval (Seq xs)


instance Show Function where
  show (Function name args funBody) =
    printf "%s(%s) =\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody)

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
          Return expr     -> makeIdent $ printf "return %s" (flatShowExpr expr)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
