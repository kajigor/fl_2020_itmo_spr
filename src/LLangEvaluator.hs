module LLangEvaluator where

import LLang
import Combinators
import Keyword
import Expr
import Control.Applicative
import AST (AST (..), Operator (..), Subst (..))
import qualified Data.Map as Map
import Data.List (intercalate)
import Expr
import LLangParser

eval :: LAst -> Configuration -> Maybe Configuration
eval (If cond thn els) c@(Conf conf input out) = do
  condRes <- evalExpr conf cond
  if condRes /= 0 then eval thn c else eval els c
eval w@(While cond body) c@(Conf conf input out) = do
  condRes <- evalExpr conf cond
  if condRes == 0 then return c else (eval body c) >>= (\c -> eval w c)
eval (Assign v expr) (Conf conf input out) = do
  n <- evalExpr conf expr
  return $ (Conf (Map.insert v n conf) input out)
eval (Read v) c@(Conf conf (x:input) out) = return $ (Conf (Map.insert v x conf) input out)
eval (Write expr) c@(Conf conf input out) = do
  n <- evalExpr conf expr
  return (Conf conf input (n:out))
eval (Seq []) conf = return conf
eval (Seq (x:xs)) conf = eval x conf >>= eval (Seq xs) 

evaluate' s input = helper $ runParser parseLLang s where
  helper (Failure _) = return []
  helper (Success _ _ ast) = do
    (Conf _ _ out) <- eval ast (Conf Map.empty input [])
    return out
  --helper (Success (InputStream e _) _ _) = error e