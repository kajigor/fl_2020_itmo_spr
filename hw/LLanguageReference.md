# L Language Reference

Пробельные символы важны только для разделения ключевых слов.
То есть табы, пробелы, newline и т.д. не влияют на поведение программы.

```bash
Language ::= Sequence
Sequence ::= Statement | Statement Sequence
Statement ::= If | While | Assign ';' | Read ';' | Write ';'
If ::= 'if' Condition '{' Sequence '}' 'else' '{' Sequence '}'
While ::= 'while' Condition '{' Sequence '}'
Assign ::= 'let' Identifier '=' Expression
Read ::= 'read' Identifier
Write ::= 'write' Expression
Expression ::= Identifier
    | Number
    | Expression Operator Expression
    | '(' Expression ')'
Condition ::= Expression Operator Expression
    | Identifier
    | '(' Condition ')'
Operator ::= '^' | '*' | '/' | '+' | '-' | '==' | '/=' | '<=' | '>=' | '>' | '<' | '&&' | '||'
Letter ::= a | b | ... | z | A | B | ... | Z
Digit ::= 0 | 1 | ... | 9
Natural = Digit | Digit Natural
Identifier ::= [a-zA-Z_][a-zA-Z0-9_]*
Number ::= '+'Natural | '-'Natural | Natural
```

 | Приоритет | Оператор             | Ассоциативность   |
 | :-------- | :------------------- | :---------------- |
 | Высший    | ^                    | Правоассоциативна |
 |           | *, /                 | Левоассоциативна  |
 |           | +, -                 | Левоассоциативна  |
 |           | ==, /=, <=, <, >=, > | Неассоциативна    |
 |           | &&                   | Правоассоциативна |
 | Низший    | \|\|                 | Правоассоциативна |
