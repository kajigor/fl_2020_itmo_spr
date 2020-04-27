### Ревью на синтаксис функций Орехова Дмитрия


1) Факториал

def factorial(n) {if (n == 1) then {return (1);} else {return (n*factorial(n-1));}}

Парсинг:

*LLang> runParser parseDef "def factorial(n) {if (n == 1) then {return (1);} else {return (n*factorial(n-1));}}"
Parsing succeeded!
Result:
factorial("n") =
|_if (n = 1)
|_then
|_|_return 1
|_else
|_|_return (n * factorial((n - 1)))

Suffix:	InputStream {stream = "", curPos = Position {line = 1, offset = 83}}


2) Числа Фибоначчи


def fib(n) {f = 1 ; g = 1; i = 1; while (i <= n - 2) {w = f; f = f + g; g = w; i = i+ 1; } return (f); }


Парсинг:

*LLang> runParser parseDef "def fib(n) {f = 1 ; g = 1; i = 1; while (i <= n - 2) {w = f; f = f + g; g = w; i = i+ 1; } return (f); }"
Parsing succeeded!
Result:
fib("n") =
|_f := 1
|_g := 1
|_i := 1
|_while (i <= (n - 2))
|_do
|_|_w := f
|_|_f := (f + g)
|_|_g := w
|_|_i := (i + 1)
|_return f

Suffix:	InputStream {stream = "", curPos = Position {line = 1, offset = 104}}


3) Поиск чисел != 0, среди данных на вход.

def find(n) { i = 1; while (i <= n) { read(x); if (x /= 0) then { write(x);} else {  c = 0;} }} def main() {read(n) ; return (find(n));}


Парсинг:

*LLang> runParser parseProg "def find(n) { i = 1; while (i <= n) { read(x); if (x /= 0) then { write(x);} else {  c = 0;} }} def main() {read(n) ; return (find(n));}"
Parsing succeeded!
Result:
find("n") =
|_i := 1
|_while (i <= n)
|_do
|_|_read x
|_|_if (x /= 0)
|_|_then
|_| |_write x
|_|_else
|_| |_c := 0


read n
return find(n)
Suffix:	InputStream {stream = "", curPos = Position {line = 1, offset = 136}}


#Баги

Багов не найдено.


#Синтаксис

Немного непривычно ставить скобки у return. 

