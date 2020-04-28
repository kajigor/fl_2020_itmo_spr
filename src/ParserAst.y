{
module ParserAst where

import Data.Char
import qualified Lex
import Text.Printf
}

%name parse
%tokentype { Lex.Token }
%error { parseError }

%token
      let             { Lex.TKw _ "let" }
      in              { Lex.TKw _ "in" }
      int             { Lex.TNum _ $$ }
      var             { Lex.TIdent _ $$ }
      '='             { Lex.TEq _ }
      '+'             { Lex.TOp _ Lex.Plus }
      '-'             { Lex.TOp _ Lex.Minus }
      '*'             { Lex.TOp _ Lex.Mult }
      '/'             { Lex.TOp _ Lex.Div }
      '('             { Lex.TLbr _ }
      ')'             { Lex.TRbr _ }

%right in
%left '+' '-'
%left '*' '/'
%%

Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
      | Exp '+' Exp             { BinOp Lex.Plus $1 $3 }
      | Exp '-' Exp             { BinOp Lex.Minus $1 $3 }
      | Exp '*' Exp             { BinOp Lex.Mult $1 $3 }
      | Exp '/' Exp             { BinOp Lex.Div $1 $3 }
      | '(' Exp ')'             { $2 }
      | int                     { Num $1 }
      | var                     { Ident $1 }

{

parseError :: [Lex.Token] -> a
parseError ts = error ("Parse error on position\n" ++ unlines (map (Lex.showPos . Lex.token_posn) ts))

data Expr = BinOp Lex.Operator Expr Expr
          | Ident String
          | Num  Int
          | Let String Expr Expr
          deriving (Eq)

instance Show Expr where
  show  = printf "\n%s" . go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          BinOp op l r -> makeIdent $ printf "%s\n%s\n%s" (showOp op) (go (ident n) l) (go (ident n) r)
          Ident x -> makeIdent $ x
          Num i -> makeIdent $ show i
          Let var value expr -> makeIdent $ printf "let %s =\n%s\n%s\n%s" var (go (ident $ ident $ ident n) value) (makeIdent $ "in") (go (ident n) expr)
      ident = (+1)
      identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
      showOp Lex.Plus = "+"
      showOp Lex.Mult = "*"
      showOp Lex.Div = "/"
      showOp Lex.Minus = "-"


runParserAst = print . parse . Lex.alexScanTokens

}