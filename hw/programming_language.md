**Язык программирования L**

**Алфавит** 
'a...bA...B0...9_'

**Структура программы**
'begin' statements 'end'

Пробелы разделяют слова языка. Отступы не имеют никакого смысла кроме визуального удобства. После каждого выражения стоит ';'.



**Идентификаторы**
'if', 'while', 'or', 'else', 'and', 'begin', 'end', 'for', 'not', 'function', 'true', 'false' etc.

num :: '-'? ('0' | ... | '9')+, целое число
var :: любое слово над алфавитом
op :: '+' | '-' | '*' | '/' | '//'
term :: '(' term op term ')' | var | num
statement :: sequence | if | while | assign | read | print
sequence :: statement ';' | sequence+
if :: 'if' '(' statement ')' 'begin' sequence 'end'
condition :: term boolean term | not term
boolean :: or | and | equals | not equals
while :: 'while' '(' condition ')' 'begin' sequence 'end'
if :: 'if' '(' condition ')' 'begin' sequence 'end'
assign :: var '=' term
or :: '||'
and :: '&&'
equals :: '=='
not equals :: '!='
not  :: '!'