{
module ParserStmts where

import Data.Char
import qualified LexStmt as Lex
import Text.Printf
}

%name parse
%tokentype { Lex.Token }
%error { parseError }

%token
      let             { Lex.TKw _ "let" }
      read            { Lex.TKw _ "read" }
      write           { Lex.TKw _ "write" }
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

S :: { [Stmt] }
  : Stmts { reverse $1 }

Stmts :: { [Stmt] }
      : Stmts Stmt {$2 : $1}
      | Stmt { [$1] }

Stmt :: { Stmt }
     : let var '=' Exp { Let $2 $4 }
     | read var { Read $2 }
     | write Exp { Write $2 }

Exp   :: { Expr }
      : Exp '+' Exp             { BinOp Lex.Plus $1 $3 }
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
          deriving (Eq)


data Stmt = Let String Expr
          | Read String
          | Write Expr

instance Show Stmt where
  show t =
    case t of
      Let var value -> printf "let %s = %s" var (show value)
      Read var -> printf "read %s" var
      Write expr -> printf "write %s" (show expr)

instance Show Expr where
  show = go
    where
      go t =
        case t of
          BinOp op l r -> printf "(%s %s %s)" (show l) (showOp op) (show r)
          Ident x -> x
          Num i -> show i
      showOp Lex.Plus = "+"
      showOp Lex.Mult = "*"
      showOp Lex.Div = "/"
      showOp Lex.Minus = "-"


runParserStmt = putStrLn . unlines . map show . parse . Lex.alexScanTokens

}