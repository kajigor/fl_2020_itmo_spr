### Ревью на язык L Боброва Артёма

## Программы

1) Факториал

read n; let i = 1; let mul = 1; while (i <= n) { let mul = mul * i; let i = i + 1; }; write(mul);


Парсинг:

*LLang> runParser parseL "read n; let i = 1; let mul = 1; while (i <= n) { let mul = mul * i; let i = i + 1; }; write(mul);"
Success "; write(mul);" (Seq {statements = [Read {var = "n"},Assign {var = "i", expr = Num 1},Assign {var = "mul", expr = Num 1},While {cond = BinOp Le (Ident "i") (Ident "n"), body = Seq {statements = [Assign {var = "mul", expr = BinOp Mult (Ident "mul") (Ident "i")},Assign {var = "i", expr = BinOp Plus (Ident "i") (Num 1)}]}}]})


2) Числа Фибоначчи
 
read n; let f = 1 ; let g = 1; let i = 1; while (i <= n - 2) {let w = f; let f = f + g; let g = w; let i = i+ 1; }; write(f);

Парсинг:

*LLang> runParser parseL "read n; let f = 1 ; let g = 1; let i = 1; while (i <= n - 2) {let w = f; let f = f + g; let g = w; let i = i+ 1; }; write(f);"
Success "; write(f);" (Seq {statements = [Read {var = "n"},Assign {var = "f", expr = Num 1},Assign {var = "g", expr = Num 1},Assign {var = "i", expr = Num 1},While {cond = BinOp Le (Ident "i") (BinOp Minus (Ident "n") (Num 2)), body = Seq {statements = [Assign {var = "w", expr = Ident "f"},Assign {var = "f", expr = BinOp Plus (Ident "f") (Ident "g")},Assign {var = "g", expr = Ident "w"},Assign {var = "i", expr = BinOp Plus (Ident "i") (Num 1)}]}}]})

3) Поиск чисел != 0, среди данных на вход.

read n; let i = 1; while (i <= n) { read x; if (x /= 0) { write(x);} else { let c = 0;} ; };

Парсинг:

*LLang> runParser parseL "read n; let i = 1; while (i <= n) { read x; if (x /= 0) { write(x);} else { let c = 0;} ; };"
Success "while (i <= n) { read x; if (x /= 0) { write(x);} else { let c = 0;} ; };" (Seq {statements = [Read {var = "n"},Assign {var = "i", expr = Num 1}]})

## Баги

Не парсятся выражения: 

*LLang> runParser parseL "while (x) {let a = 3;};"
Success ";" (Seq {statements = [While {cond = Ident "x", body = Seq {statements = [Assign {var = "a", expr = Num 3}]}}]})


*LLang> runParser parseL "if (x /= 0 ) { write(x);} else { let c = 0;};"
Success ";" (Seq {statements = [If {cond = BinOp Nequal (Ident "x") (Num 0), thn = Seq {statements = [Write {expr = Ident "x"}]}, els = Seq {statements = [Assign {var = "c", expr = Num 0}]}}]})

Если убрать точку с запятой
в конце while и if в примерах выше, то все парсится. 
А по документации после каждого Statement должна стоять ';'. 


Я сказала это Артёму, он исправил в HW07.
Теперь: Statement ::= If | While | Assign ';' | Read ';' | Write ';'


## Документация и синтаксис

В документации вроде всё хорошо. Синтаксис показался удобным, особенно потому, что 
поддержаны пробелы в любом месте и символы новой строки ('\n').


Возможно, можно было бы обойтись без let в присваивании,
чтобы не увеличивать длину выражений. 
Еще хотелось бы возможность пустой ветки else {},
Так как по документации Sequence не пустой, то приходится в else что-то писать,
даже если это не нужно.



