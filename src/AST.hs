module AST where

import Text.Printf (printf)

data Operator = Plus
              | Mult
              | Minus
              | Div
              | Pow
              | And
              | Or
              | Equal
              | Nequal
              | Lt
              | Gt
              | Ge
              | Le
              | Not
              | UnaryMinus
              deriving (Eq)



data AST = BinOp Operator AST AST
         | UnaryOp Operator AST
         | Num Int
         | Ident String
         deriving (Eq)

instance Show Operator where
  show Plus = "+"
  show Mult = "*"
  show Minus = "-"
  show Div = "/"
  show Pow = "^"
  show And = "&&"
  show Or = "||"
  show Equal = "=="
  show Nequal = "/="
  show Lt = "<"
  show Gt = ">"
  show Ge = ">="
  show Le = "<="
  show Not = "!"
  show UnaryMinus = "-"


instance Show AST where
  show  = printf "\n%s" . go 0
    where
      go n t =
        (if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id) $
        case t of
          BinOp op l r -> printf "%s\n%s\n%s" (show op) (go (ident n) l) (go (ident n) r)
          Num i -> show i
          Ident i -> show i
          UnaryOp op e -> printf "%s\n%s\n%s" (show op) (go (ident n) e) 
      ident = (+1)
