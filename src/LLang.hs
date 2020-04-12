module LLang where

import AST (AST (..), Operator (..), Subst(..))
import Combinators (Parser (..), Result (..), symbol, fail')
import qualified Data.Map as Map
import Data.List (intercalate)
import Text.Printf (printf)

import Expr (evalExpr, numToBool, parseExpr, parseIdent)
import Control.Applicative(Alternative (..))
import Control.Monad (foldM)
import Keyword

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: Expr, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving Eq

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


lLangKW :: Parser String String String
lLangKW = keyword ["if",
                   "then",
                   "else",
                   "while",
                   "read",
                   "write"]

parseVarName = do
  varName <- parseIdent
  case runParser lLangKW varName of
    Failure _ -> return varName
    _ -> fail' "Variable name can't be a language keyword"
    

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
  parseAssign = manySpace *> (Assign <$> parseVarName <* assignOpParser <*> parseExpr) <* semiOpParser
  parseCond = manySpace *> (If <$> (parseString "if" *> parseRound parseExpr) <*> 
                                   (parseString "then" *> parseCurly parseL')  <*>
                                   (parseString "else" *> parseCurly parseL'))
  parseWhile = manySpace *> (While <$> (parseString "while" *> parseRound parseExpr) <*>
                                       parseCurly parseL')
  parseWrite = manySpace *> (Write <$> (parseString "write" *> parseRound parseExpr)) <* semiOpParser
  parseRead = manySpace *> (Read <$> (parseString "read" *> someSpace *> parseIdent)) <* semiOpParser


initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval (If cond thn els) conf@(Conf map _ _) = do
  cond' <- evalExpr map cond
  if numToBool cond'
  then eval thn conf
  else eval els conf
eval wh@(While cond body) conf@(Conf map _ _) = do
  cond' <- evalExpr map cond
  if numToBool cond'
  then do
    conf' <- eval body conf
    eval wh conf'
  else return conf
eval (Assign var expr) (Conf map inp out) = do
  expr' <- evalExpr map expr
  return $ Conf (Map.insert var expr' map) inp out
eval (Read var) (Conf map inp out) = case inp of
  [] -> Nothing
  (x:xs) -> return $ Conf (Map.insert var x map) xs out
eval (Write expr) (Conf map inp out) = do
  expr' <- evalExpr map expr
  return $ Conf map inp (expr':out)
eval (Seq ls) conf = foldM (flip eval) conf ls

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
