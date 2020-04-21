## Language L

Пробелы допустимы везде кроме выражений с операторами, например, должно быть так: x&&y, 1+2, a==b.
А не так:  x && y, 1  + 2, a  ==  b. 
Пробелы отделяют одно слово от другого и больше никакого смысла не несут.
Cимволы переноса строки '\n' допустимы там же, где допустимы пробелы и несут такой же смысл, что и пробелы.

Структура программы:

Programm :: Function; | Function; Programm

Function :: def Ident '(' Args ')' { Seq }

Args :: Ident | Ident, Args

Seq :: Statement ';' | Statement ';' Seq

Statement :: If | While | Initialize | Read | Write | FuncCall

FuncCall :: Ident '(' CallArgs ')'

CallArgs :: Expression | Expression, CallArgs

Read :: 'read' '(' Ident ')'

Write :: 'write' '(' Expression ')'

If :: 'if' '(' Condition ')' '{' Seq '}' 'else' '{' Seq '}'

While :: 'while' '(' Condition ')' '{' Seq '}'

Initialize :: Ident '=' Expression

Expression ::= Ident | Integer | FuncCall | Expression LogikOperator Expression  | Expression ArithmeticOperator Expression | '(' Expression ')'

Condition ::= Expression LogikOperator Expression | 'true' | 'false' | LogikOperatorUnary '(' Condition ')'

LogikOperator :: '&&' | '||' |  '==' | '/=' | '>' | '<' | '<=' | '>='

LogikOperatorUnary :: '!' 

ArithmeticOperator :: '+' | '-' | '*' | '/' | '^' |
LetterSmall :: 'a' | ... | 'z' 
LetterBig :: 'A' | ... | 'Z'
Digit :: '0' | ... | '9'
		
Num :: '-' Num | Digit+
Symbol ::  LetterSmall | LetterBig | '_'
Ident :: Symbol | Ident Digit | Ident Symbol
 
```
 | Приоритет | Оператор             | Ассоциативность   |
 | :-------- | :------------------- | :---------------- |
 | Высший    | ^                    | Правоассоциативна |
 |           | *, /                 | Левоассоциативна  |
 |           | +, -                 | Левоассоциативна  |
 |           | ==, /=, <=, <, >=, > | Неассоциативна    |
 |           | !                    |  --- 
 |           | &&                   | Правоассоциативна |
 | Низший    | \|\|                 | Правоассоциативна |
