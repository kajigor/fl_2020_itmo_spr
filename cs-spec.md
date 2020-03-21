# concrete syntax spec

```
STMT = IF | WHILE | ASSIGN | READ | WRITE | SEQ
IF = 'if' EXPR STMT STMT
WHILE = '(' 'while' EXPR STMT ')'
ASSIGN = '(' ':=' IDENT EXPR ')'
READ = '(' 'read' IDENT ')'
WRITE = '(' EXPR ')'
SEQ = '(' 'do' ('(' STMT ')')* ')'
EXPR = EXPR OP EXPR
     | IDENT
     | NUMBER
OP = '+' | '-' | '*'| '/' | '^' | '==' | '!=' | '>' | '>=' | '<' | '<=' | '&&' | '||'
IDENT = ('a' | .. | 'z') ('a' | .. | 'z' | '0' .. '9')+
NUMBER = '-'? ('0' | .. | '9')+
```
