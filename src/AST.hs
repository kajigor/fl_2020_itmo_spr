module AST where

import qualified Data.Map    as Map
import           Text.Printf (printf)

data Operator = Plus
              | Mult
              | Minus
              | Div
              | Pow
              | Equal
              | Nequal
              | Gt
              | Ge
              | Lt
              | Le
              | And
              | Or
              | Not
              deriving (Eq)

type Subst = Map.Map String Int

data AST = BinOp Operator AST AST
         | UnaryOp Operator AST
         | Ident String
         | Num Int
         deriving (Eq)

instance Show Operator where
  show Plus   = "+"
  show Mult   = "*"
  show Minus  = "-"
  show Div    = "/"
  show Equal  = "="
  show Pow    = "^"
  show Nequal = "/="
  show Gt     = ">"
  show Ge     = ">="
  show Lt     = "<"
  show Le     = "<="
  show And    = "&&"
  show Or     = "||"
  show Not    = "!"


instance Show AST where
  show  = printf "\n%s" . go 0
    where
      go n t =
        (if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id) $
        case t of
          BinOp op l r -> printf "%s\n%s\n%s" (show op) (go (ident n) l) (go (ident n) r)
          UnaryOp op x -> printf "%s\n%s" (show op) (go (ident n) x)
          Ident x -> x
          Num i -> show i
      ident = (+1)
