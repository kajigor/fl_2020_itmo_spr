module AST where

import Text.Printf (printf)

data Operator = Plus
              | Mult
              | Minus
              | Div
              deriving (Eq)

data AST = BinOp Operator AST AST
         | Num Int
         deriving (Eq)

instance Show Operator where
  show Plus = "+"
  show Mult = "*"
  show Minus = "-"
  show Div = "/"

instance Show AST where
  show  = printf "\n%s" . go 0
    where
      go n t =
        (if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id) $
        case t of
          BinOp op l r -> printf "%s\n%s\n%s" (show op) (go (ident n) l) (go (ident n) r)
          Num i -> show i
      ident = (+1)
