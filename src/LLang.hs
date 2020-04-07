module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators (Parser (..))
import qualified Data.Map as Map

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
  deriving (Show, Eq)

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

parseL :: Parser String String LAst
parseL = error "parseL undefined"

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval = error "eval not defined"