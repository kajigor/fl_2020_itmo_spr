grammar CFGrammar;

@header {
package antlr;
}

cfgrammar: cfrules EOF;

cfrules: (cfrule SEMICOLON)*;

cfrule: nonterm ASSIGN rvalue;

nonterm: id;

rvalue: alternatives;

alternatives: alternative (BAR alternative)*;

alternative: element+;

element: text | nonterm;

text: STRING;

id: LT ruleid GT;

ruleid: ID;

ASSIGN: '::=';

BAR: '|';

GT: '>';

LT: '<';

QUOTE: '\'';

SEMICOLON: ';';

STRING: QUOTE CHAR* QUOTE;

ID: ('a' ..'z' | 'A' ..'Z') CHAR+;

fragment CHAR: (
		'a' ..'z'
		| 'A' ..'Z'
		| '0' ..'9'
		| '-'
		| '_'
		| ' '
	);

WS: [ \r\n\t] -> skip;
