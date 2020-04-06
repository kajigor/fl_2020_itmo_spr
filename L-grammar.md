## **L** is for stripped down [Lua]

### Grammar
```
digit ::= '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0'

num ::= digit | digit num

alphabet ::= [A-Za-z_]
tick ::= '\''
chars ::= alphabet | alphabet chars

string = '\"' chars '\"'

var ::= chars | var tick

statements ::= statement ';' | statement ';' statements

statement ::= 'if' condition 'then' dodone { 'elif' condition dodone } ['else' dodone]
            | 'while' condition dodone
            | 'let' VARNAME '=' expression
            | 'let' 'function' VARNAME params dodone    -- function definition
            | 'function' args                           -- function call 

condition ::= '(' expression ')'
dodone ::= 'do' statements 'done'
params ::= '(' VARNAME { ',' VARNAME } ')'
args ::= '(' expression { ',' expression } ')'

```

[Lua]: http://lua-users.org/wiki/LuaFourOneGrammar
