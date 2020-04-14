### Ревью на язык L Цыкунова Дмитрия

## Программы
Было не очень понятно, где можно ставить пробелы, а где -- нет. Поэтому пробелы были поставлены произвольно.

1) Факториал

let function(n) do let i=1; let mul=1; while(i<=n) do let mul=mul*i;let i=i+1; done; write(mul); done;

Парсинг:

*LLang> runParser parseL "let function(n) do let i=1; let mul=1; while(i<=n) do let mul=mul*i;let i=i+1; done; write(mul); done;"
Success "let function(n) do let i=1; let mul=1; while(i<=n) do let mul=mul*i;let i=i+1; done; write(mul); done;" (Seq {statements = []})

2) Числа Фибоначчи

read(n);let f=1;let g=1;let i=1; while(i<=n-2) do let w=f;let f=f+g;let g=w; let i=i+1; done; write(f);

Парсинг:

*LLang> runParser parseL "read(n);let f=1;let g=1;let i=1; while(i<=n-2) do let w=f;let f=f+g;let g=w; let i=i+1; done; write(f);"
Success "read(n);let f=1;let g=1;let i=1; while(i<=n-2) do let w=f;let f=f+g;let g=w; let i=i+1; done; write(f);" (Seq {statements = []})

3) Поиск чисел != 0, среди данных на вход.

read(n); let i=1; while(i<=n) do read(x); if (x/=0) then write(x); done;

Парсинг:

*LLang> runParser parseL "read(n); let i=1; while(i<=n) do read(x); if (x!=0) then write(x); done;"
Success "read(n); let i=1; while(i<=n) do read(x); if (x/=0) then write(x); done;" (Seq {statements = []})


## Баги

Почти ничего не парсится.
Удалось распарсить только:

let a=2;let b=3;let c=a;

*LLang> runParser parseL "let a=2;let b=3;let c=a;"
Success ";" (Seq {statements = [Let {var = "a", expr = 
2},Let {var = "b", expr = 3},Let {var = "c", expr = a}]})

Правда, ";" все равно осталась.


## Документация и синтаксис

Из контекста понятно, что для парсинга expressions используеются parseExpr,
но в документации не указано что такое expression, а также операторы, которые можно в них использовать.
Еще в документации не сказано, какой смысл несут пробелы.

В синтаксисе показалось неудобным присваивание: let x=3; Хотелось бы, чтобы можно было ставить пробелы
слева и справа от знака равно. (с пробелами не парсилось).


Ссылка на баги в репозитории: https://github.com/ystid/fl_2020_ifmo_spr/issues/2


