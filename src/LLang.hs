module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators (Parser (..), runParser, Result(..), InputStream(..), getPos, makeError, symbol, fail', (<?>), sepBy1)
import qualified Data.Map as Map
import Data.List (intercalate, partition)
import Text.Printf (printf)
import Expr
import Control.Applicative(Alternative (..))
import Control.Monad (foldM)
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
parseDef = Function <$> parseFuncName <*> parseArgs <*> parseCurly parseL where
  parseFuncName = manySpace *> (parseString "def" *> someSpace *> parseVarName)
  parseArgs = parseRound $ sepBy1 commaOpParser parseVarName <|> pure []
  parseBody = parseCurly $ parseL <|> pure (Seq [])

parseProg :: Parser String String Program
parseProg = do
  funcs <- some parseDef
  let isMain (Function name _ _) = name == "main"
      (body, defs) = partition isMain funcs
  if length body == 0
  then "Main function is absent" <?> empty
  else if length body > 1
       then "Multiple definitions of main function" <?> empty
       else return $ Program defs (funBody $ head body)

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

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

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

lLangKW :: Parser String String String
lLangKW = keyword ["if",
                   "then",
                   "else",
                   "while",
                   "read",
                   "write",
                   "return",
                   "def"]

--parseVarName :: 
parseVarName = do
  pos <- getPos
  varName <- parseIdent
  "Keyword can't be an identificator" <?> case runParser lLangKW varName of
    Failure _ -> return varName
    _ -> empty
    

spaceChars = (symbol ' ' <|> symbol '\t' <|> symbol '\n')
manySpace = many spaceChars
someSpace = some spaceChars

assignOpParser = manySpace *> symbol '=' <* manySpace
semiOpParser = manySpace *> symbol ';' <* manySpace
commaOpParser = manySpace *> symbol ',' <* manySpace

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
  parseStatement = parseAssign <|> parseCond <|> parseWhile <|> parseWrite <|> parseRead <|> parseReturn
  parseAssign = manySpace *> (Assign <$> parseVarName <* assignOpParser <*> parseExpr) <* semiOpParser
  parseCond = manySpace *> (If <$> (parseString "if" *> parseRound parseExpr) <*> 
                                   (parseString "then" *> parseCurly parseL')  <*>
                                   (parseString "else" *> parseCurly parseL'))
  parseWhile = manySpace *> (While <$> (parseString "while" *> parseRound parseExpr) <*>
                                       parseCurly parseL')
  parseWrite = manySpace *> (Write <$> (parseString "write" *> parseRound parseExpr)) <* semiOpParser
  parseRead = manySpace *> (Read <$> (parseString "read" *> manySpace *> parseRound parseIdent)) <* semiOpParser
  parseReturn = manySpace *> (Return <$> (parseString "return" *> manySpace *> parseRound parseExpr)) <* semiOpParser

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

