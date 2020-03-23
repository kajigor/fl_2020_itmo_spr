#Язык L

Пробелы переносы строк используются только, чтобы разделять лексемы языка.
Например, if a не тоже самое, что ifa. То есть пробельные символы нужны лексеру,
но при парсинге они игнорируются

##Граматика
###Макросы

a+ == X, где X - дополнительно введеное правило X -> a | a X 
   a* == X, где X - дополнительно введеное правило X -> a X | eps
[a|b|...] == X, где X - дополнительно введеное правило X -> a | b | ...

Для каждого использования макроса предпалагается, что X раннее не используемое правило 

	
Digit -> '0' | '1' ... | '9'		
Number -> '-' Number | Digit+

Letter -> SmallLetter | BigLetter

SmallLetter -> 'a' | ... | 'z'

BigLetter -> 'A' | ... | 'Z'

Ident -> [SmallLetter|'_'] [Letter|Digit|'_']*

Keywords -> 'if' | 'while' | 'else' | 'read' | 'write' 

Expr ->  Or

Term -> Number | Ident | '(' Expr ')'

Or -> Or '||' And | And

And -> And '^^' Eq | Eq

Eq -> SumOrMinus ['==' | '/=' | '>' | '<' | '>=' | '<='] SumOrMinus | SumOrMinus

SumOrMinus -> MultOrDiv ['+'|'-'] SumOrMinus | MultOrDiv

MultOrDiv -> MultOrDiv ['*'|'-'] Pow | Pow

Pow -> Term '^' Term | Term


If -> 'if' '(' Expr ')' '{' Statements '}' 'else' '{' Statements '}' 

While -> 'while' '(' Expr ')' '{' Statement+ '}'

Assign -> ident '=' Expr

Read -> 'read' '(' Ident ')'

Write -> 'write' '(' Expr ')'

Statement = If | While | Assign | Read | Write 

Statements = Statement | Statement ';' Statements
