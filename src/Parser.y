{ -- happy
module Parser where

import Data.Char
import qualified Lex
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

%%

Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
      | Exp1                    { Exp1 $1 }

Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor
      : int                     { Int $1 }
      | var                     { Var $1 }
      | '(' Exp ')'             { Brack $2 }

{

parseError :: [Lex.Token] -> a
parseError ts = error ("Parse error on position\n" ++ unlines (map (Lex.showPos . Lex.token_posn) ts))

data Exp
      = Let String Exp Exp
      | Exp1 Exp1
      deriving Show

data Exp1
      = Plus Exp1 Term
      | Minus Exp1 Term
      | Term Term
      deriving Show

data Term
      = Times Term Factor
      | Div Term Factor
      | Factor Factor
      deriving Show

data Factor
      = Int Int
      | Var String
      | Brack Exp
      deriving Show

runParser = print . parse . Lex.alexScanTokens

}