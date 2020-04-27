### Ревью на синтаксис функций Боброва Артёма

## Программы
Было не очень понятно, где можно ставить пробелы, а где -- нет. Поэтому пробелы были поставлены произвольно.

1) Факториал

func factorial(n) {if (n == 1) {return 1;} else {return n*factorial(n-1);}}

Парсинг:

*LLang> runParser parseDef "func factorial(n) {if (n == 1) {return 1;} else {return n*factorial(n-1);}}"
Parsing succeeded!
Result:
factorial("n") =
|_if (n = 1)
|_then
|_|_return 1
|_else
|_|_return (n * factorial((n - 1)))

Suffix:	InputStream {stream = "", curPos = Position {line = 0, column = 75}}



2) Числа Фибоначчи



