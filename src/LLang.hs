{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LLang where

import AST
import UberExpr
import Combinators
import qualified Data.Map as Map
import Data.List (intercalate)
import Text.Printf (printf)
import Control.Monad
import Prelude hiding (seq, read)
import Control.Applicative
import Data.Char
import Control.Applicative
import Data.Foldable

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
  deriving (Eq)

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

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

assign = Assign <$> ident <*> (spc *> string ":=" *> spc *> parseExpr)
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

string str = sequence $ map (\c -> satisfy (c ==)) str



-- Парсер арифметических выражений над целыми числами с операциями +,-,*,/.
parseExpr :: Parser String String AST
parseExpr =
  uberExpr
  [ (string "||" *> pure Or,                    Binary RightAssoc)
  , (string "&&" *> pure And,                   Binary RightAssoc)
  , (symbol '!' *> pure Not,                    Unary)
  , (asum [ string "==" *> pure Equal
          , string "/=" *> pure Nequal
          , string "<=" *> pure Le
          , symbol          '<' *> pure Lt
          , string ">=" *> pure Ge
          , symbol          '>' *> pure Gt
          ],                                    Binary NoAssoc)
  , (symbol '+' *> pure Plus <|>
     symbol '-' *> pure Minus,                  Binary LeftAssoc)
  , (symbol '*' *> pure Mult <|>
     symbol '/' *> pure Div,                    Binary LeftAssoc)
  , (symbol '-' *> pure Minus,                  Unary)
  , (symbol '^' *> pure Pow,                    Binary RightAssoc)
  ]

  ( (Num <$> parseNum) <|>
    (Ident <$> parseIdent) <|>
    (symbol '(' *> parseExpr <* symbol ')')
  )

  BinOp
  UnaryOp


-- Парсер чисел
parseNum :: Parser String String Int
parseNum =
  number <|> symbol '-' *> (negate <$> number)
  where
    number = fmap toNum go
    digit = satisfy isDigit
    toNum = foldl (\acc d -> 10 * acc + digitToInt d) 0
    go = some digit

parseIdent :: Parser String String String
parseIdent = liftM2 (:) alpha (many alphaNumeric)
  where
    alpha :: Parser String String Char
    alpha = satisfy (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_')
    alphaNumeric :: Parser String String Char
    alphaNumeric = alpha <|> satisfy (\c -> c >= '0' && c <= '9')


eval :: LAst -> Configuration -> Maybe Configuration
eval (If {thn, els, cond}) cfg = do
  Left c <- evalAST cfg cond
  if c then eval thn cfg else eval els cfg
eval e@(While {cond, body}) cfg = do
  Left c <- evalAST cfg cond
  if c then eval body cfg >> eval e cfg else pure cfg
eval e@(Assign { var, expr }) cfg@(Conf { subst, input, output }) = do
  Right val <- evalAST cfg expr
  pure $ Conf (Map.insert var val subst) input output
eval e@(Read var) cfg@(Conf { subst, input= (v:vs), output }) = do
  pure $ Conf (Map.insert var v subst) vs output
eval e@(Write expr) cfg@(Conf { subst, input, output }) = do
  Right n <- evalAST cfg expr
  pure $ Conf subst input (n:output)
eval e@(Seq { statements = [] }) cnf = pure cnf
eval e@(Seq { statements = (s:ss) }) cnf = eval s cnf >>= eval (e { statements = ss })

loopWhileM :: Monad m => m Bool -> m () -> m ()
loopWhileM pr act = do
    b <- pr
    when b $ do
      act
      loopWhileM pr act


evalAST :: Configuration -> AST -> Maybe (Either Bool Int)
evalAST cfg (UnaryOp op a) = cont
  where
    cont = case op of
      Minus -> go1 negate
      Not -> go2 not
    go1 f = do
      Right n <- evalAST cfg a
      pure $ Right $ f n
    go2 :: (Bool -> Bool) -> Maybe (Either Bool Int)
    go2 f = do
      Left n <- evalAST cfg a
      pure $ Left $ f n
evalAST (Conf { subst }) (Ident var) =
  Right <$> Map.lookup var subst
evalAST _ (Num n) = pure $ Right n
evalAST cfg (BinOp op l r) = cont l r
  where
    cont = case op of
      Plus -> withBinaryInt (+)
      Mult -> withBinaryInt (*)
      Minus -> withBinaryInt (-)
      Div -> withBinaryInt div
      Pow -> withBinaryInt (^)
      Equal -> withBinaryIntBool (==)
      Nequal -> withBinaryIntBool (/=)
      Gt -> withBinaryIntBool (>)
      Ge -> withBinaryIntBool (>=)
      Lt -> withBinaryIntBool (<)
      Le -> withBinaryIntBool (<=)
      And -> withBinaryBool (&&)
      Or -> withBinaryBool (||)
      Not -> mempty

    withBinaryInt :: (Int -> Int -> Int) -> AST -> AST -> Maybe (Either Bool Int)
    withBinaryInt f l r = do
      Right ln <- evalAST cfg l
      Right rn <- evalAST cfg r
      pure $ Right $ f ln rn

    withBinaryBool :: (Bool -> Bool -> Bool) -> AST -> AST -> Maybe (Either Bool Int)
    withBinaryBool f l r = do
      Left ln <- evalAST cfg l
      Left rn <- evalAST cfg r
      pure $ Left $ f ln rn

    withBinaryIntBool :: (Int -> Int -> Bool) -> AST -> AST -> Maybe (Either Bool Int)
    withBinaryIntBool f l r = do
      Right ln <- evalAST cfg l
      Right rn <- evalAST cfg r
      pure $ Left $ f ln rn

evalExpr :: Subst -> AST -> Maybe Int
evalExpr s a = do
  Right n <- evalAST (Conf s [] []) a
  pure n
