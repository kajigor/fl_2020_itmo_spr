{
module PParser where

import Data.List (intercalate)
import qualified PLex
import Text.Printf
}

%name parse
%tokentype { PLex.Token }
%error { parseError }

%token
      ident  { PLex.TId $$ }
      var    { PLex.TVar $$ }
      '?-'   { PLex.TTrg }
      ':-'   { PLex.TNeck }
      ','    { PLex.TCom }
      '.'    { PLex.TPrd }
      '('    { PLex.TLbr }
      ')'    { PLex.TRbr }
%%

Program :: { P }
        : Relations '?-' Task '.' { P (reverse $1) $3 }

Relations :: { [Line] }
          : Relations Rltn     { $2 : $1 }
          | {- empty -}        { [] }

Rltn :: { Line }
     : Atom Body '.'           { Line $1 $2 }

Args :: { [Arg] }
     : Args ',' Arg            { $3 : $1 }
     | Arg                     { [$1] }

Arg :: { Arg }
    : var                      { V $1 }
    | Atom                     { A $1 }

Atom :: { Atom }
     : ident '(' Args ')'      { Atom $1 (reverse $3) }
     | ident                   { Atom $1 [] }

Body :: { Body }
     : ':-' Atoms              { Body (reverse $2) }
     | {- empty -}             { Body  [] }

Atoms :: { [Atom] }
      : Atoms ',' Atom         { $3 : $1 }
      | Atom                   { [$1] }

Task :: { Task }
     : Atoms                   { Task (reverse $1) }
     | {- empty -}             { Task [] }

{

commaFold :: [String] -> String
commaFold xs = intercalate ", " xs

data P = P [Line] Task deriving (Eq)

instance Show P where
    show (P lines task) = printf "%s ?- %s." (intercalate "\n" (map show lines)) (show task)

data Line = Line Atom Body deriving (Eq)

instance Show Line where
    show (Line head body@(Body b)) | null b = printf "%s." (show head)
                                   | otherwise = printf "%s :- %s." (show head) (show body)

type Ident = String 

type Var = String 

{- type Arg = Either Var Atom -}

data Helper a b = V a | A b deriving (Eq)
type Arg = Helper Var Atom

instance (Show a, Show b) => Show (Helper a b) where
    show x = case x of
        V a -> show a
        A b -> show b

data Body = Body [Atom] deriving (Eq)

instance Show Body where
    show x = case x of
        Body b -> if null b 
                  then printf ""
                  else printf "%s" (commaFold (map show b))

data Atom = Atom Ident [Arg] deriving (Eq)

instance Show Atom where
    show x = case x of
        Atom id args -> if null args
                        then printf "%s" id
                        else printf "%s(%s)" id (commaFold (map show args))

data Task = Task [Atom] deriving (Eq)
instance Show Task where
    show x = case x of
        Task b -> if null b 
                  then printf ""
                  else printf "%s" (commaFold (map show b))

parseError :: [PLex.Token] -> a
parseError ts = error ("parse error on token" ++ (show ts))

runParser = print . parse . PLex.alexScanTokens

orig = 
    [ "eval(St, var(X), U) :- elem(X, St, U)."
    , "eval(St, conj(X,Y), U) :- eval(St, X, V), eval(St, Y, W), and(V, W, U)."
    , "eval(St, disj(X,Y), U) :- eval(St, X, V), eval(St, Y, W), or(V, W, U)."
    , "eval(St, not(X), U) :- eval(St, X, V), neg(U, V)."
    , "elem(zero, cons(H,T), H)."
    , "elem(succ(N), cons(H,T), V) :- elem(N, T, V)."
    , "nand(false, false, true)."
    , "nand(false, true, true)."
    , "nand(true, false, true)."
    , "nand(true, true, false)."
    , "neg(X, R) :- nand(X, X, R)."
    , "or(X, Y, R) :- nand(X, X, Xx), nand(Y, Y, Yy), nand(Xx, Yy, R)."
    , "and(X, Y, R) :- nand(X, Y, Xy), nand(Xy, Xy, R)."
    , "?- eval(St, conj(disj(X,Y),not(var(Z))), true)."
    ]

}
