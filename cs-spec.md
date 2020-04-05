# concrete syntax spec

Пробелы допустимы везде, кроме арифметических выражений.

```bash
LANG := SEQ
STMT := IF | WHILE | ASSIGN | READ | WRITE | SEQ
IF := 'if' '(' EXPR ')' '{' SEQ '}' '{' SEQ '}'
WHILE := 'while' '(' EXPR ')' '{' SEQ '}'
ASSIGN := IDENT ':=' EXPR
READ := 'read' IDENT
WRITE := 'write' EXPR
SEQ := ( '(' STMT ')' )*
EXPR := OPEXPR1
OPEXPR1 := OPEXPR2 ('||' OPEXPR2)*
OPEXPR2 := OPEXPR3 ('&&' OPEXPR3)*
OPEXPR3 := OPEXPR4 (('==' | '/=' | '<=' | '<' | '>=' | '>') OPEXPR4)*
OPEXPR4 := OPEXPR5 (('+' | '-') OPEXPR5)*
OPEXPR5 := OPEXPR6 (('*' | '/') OPEXPR6)*
OPEXPR6 := OPEXPR7 ('^' OPEXPR7)*
OPEXPR7 := '(' EXPR ')' | IDENT | NUMBER
OP := '+' | '-' | '*'| '/' | '^' | '==' | '!=' | '>' | '>=' | '<' | '<=' | '&&' | '||'
IDENT :=
  ('a' | .. | 'z' | 'A' | .. | 'Z' | '_')
  ('a' | .. | 'z' | 'A' | .. | 'Z' | '_' | '0' .. '9')*
NUMBER := '-'? ('0' | .. | '9')+
```
