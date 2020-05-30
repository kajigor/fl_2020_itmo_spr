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
     : Head Body '.'           { Line $1 $2 }

Head :: { Head }
     : ident '(' Args ')'      { Head $1 (reverse $3) }

Args :: { [Arg] }
     : Args ',' Arg            { $3 : $1 }
     | Arg                     { [$1] }

Arg :: { Arg }
    : var                      { Left $1 }
    | Atom                     { Right $1 }

Atom :: { Atom }
     : ident '(' Args ')'      { Atom $1 $3 }
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

data P = P [Line] Task

instance Show P where
    show (P lines task) = printf "%s ?- %s." (intercalate "\n" (map show lines)) (show task)

data Line = Line Head Body

instance Show Line where
    show (Line head body@(Body b)) | null b = printf "%s." (show head)
                                   | otherwise = printf "%s :- %s." (show head) (show body)


data Head = Head Ident [Arg]

instance Show Head where
    show (Head id args) | null args = printf "%s" id
                        | otherwise = printf "%s(%s)" id (commaFold (map show args))

type Ident = String 

type Var = String 

type Arg = Either Var Atom

data Body = Body [Atom]

instance Show Body where
    show x = case x of
        Body b -> if null b 
                  then printf ""
                  else printf "%s" (commaFold (map show b))

data Atom = Atom Ident [Arg]

instance Show Atom where
    show x = case x of
        Atom id args -> if null args
                        then printf "%s" id
                        else printf "%s(%s)" id (commaFold (map show args))

data Task = Task [Atom]
instance Show Task where
    show x = case x of
        Task b -> if null b 
                  then printf ""
                  else printf "%s" (commaFold (map show b))

parseError = undefined

runParser = print . parse . PLex.alexScanTokens

}
