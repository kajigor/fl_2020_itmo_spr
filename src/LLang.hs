module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators (Parser (..))
import qualified Data.Map as Map
import Data.List (intercalate)
import Text.Printf (printf)
import Expr
import Control.Applicative(Alternative (..))
import Keyword

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

parseDef :: Parser String String Function
parseDef = error "parseDef undefined"

parseProg :: Parser String String Program
parseProg = error "parseProg undefined"

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

<<<<<<< HEAD
parseL :: Parser String String LAst
parseL = error "parseL undefined"

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval = error "eval not defined"

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
=======
spaceChars = (symbol ' ' <|> symbol '\t' <|> symbol '\n')
manySpace = many spaceChars
someSpace = some spaceChars

assignOpParser = manySpace *> symbol '=' <* manySpace
semiOpParser = manySpace *> symbol ';' <* manySpace

-- Показалось, что так будет удобнее, чем через keywords.
-- Все равно нам нужно не просто считать ключевое слово,
-- но и убедиться, что считалось какое-то конкретное
parseString :: String -> Parser String String String
parseString [] = pure ""
parseString (x:xs) = (:) <$> symbol x <*> parseString xs

parseBrackets (op,cl) p = (manySpace *> symbol op <* manySpace) 
                          *> p <*
                          (manySpace *> symbol cl <* manySpace)
parseRound = parseBrackets ('(', ')')
parseCurly = parseBrackets ('{', '}')

parseL :: Parser String String LAst
parseL = lFromList <$> some parseStatement where
  lFromList :: [LAst] -> LAst
  lFromList [x] = x
  lFromList xs = Seq xs
  -- To allow for empty blocks in while and if
  parseL' = parseL <|> (const (Seq []) <$> manySpace)
  parseStatement :: Parser String String LAst
  parseStatement = parseAssign <|> parseCond <|> parseWhile <|> parseWrite <|> parseRead
  parseAssign = manySpace *> (Assign <$> parseIdent <* assignOpParser <*> parseExpr) <* semiOpParser
  parseCond = manySpace *> (If <$> (parseString "if" *> parseRound parseExpr) <*> 
                                   (parseString "then" *> parseCurly parseL')  <*>
                                   (parseString "else" *> parseCurly parseL'))
  parseWhile = manySpace *> (While <$> (parseString "while" *> parseRound parseExpr) <*>
                                       parseCurly parseL')
  parseWrite = manySpace *> (Write <$> (parseString "write" *> parseRound parseExpr)) <* semiOpParser
  parseRead = manySpace *> (Read <$> (parseString "read" *> someSpace *> parseIdent)) <* semiOpParser
>>>>>>> First commit
