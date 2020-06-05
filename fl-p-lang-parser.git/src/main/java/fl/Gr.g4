grammar Gr;
@header {
package fl;
}

program: relation* '?-' target ;

target: '.' | atom ',' target | atom '.';

relation: head ':-' body | head '.' ;

head: atom ;

body: '.' | atom '.' | atom ',' body ;

arg: atom | Var ;

atom: Ident '(' args ')' | Ident;

args: arg ',' args | arg ;

Ident: [a-z][a-zA-Z0-9]* ;
Var: [A-Z][a-zA-Z0-9]* ;

WS  :   (' ' | '\t' | '\n')+ {skip();};