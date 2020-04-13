### Язык LLang [Жени(dedok1997)](https://github.com/dedok1997/fl_2020_ifmo_spr)

Синтаксис:

* После тела `while`, `if` надо ставить `;`

Парсинг:

~* Какие-то проблемы с пробелами:~(Женя сказал, что это не баг)

~Не парсится:~
~```a=2 + 2;```~
~```python~
~a=b  ==  1;~
~```~
~Хотя в тоже время принимается:~
~```python~
~a = 2+2;~
~```~
~```python~
~a=b==1;~
~```~
~* Не парсится.~(Да, согласно грамматике это валидно, но, стоит отметить, это достаточно необычно)

Такое ощущение, что в теле if'а и while'a должен быть хотя бы 1 statement. ~
~```python~
~while(a==1){};~
~```~
~```~
~if(a==2){}else{b=3;};~
~```~


Написанные программы:
* `factorial.llang` Печатает факториал заданного числа

Запуск из файла:
```haskell
(flip evaluate' [5]) <$> (readFile "fac.llang")
```

```python
fac = 1;
i = 1;
read(number);
number = 5;
while (i <= number) {
    fac = fac * i;
    i = i+1;
};
write(fac);
```
* `fibonaci.llang` Печатает заданное число Фибоначчи 

```haskell
(flip evaluate' [8]) <$> (readFile "fibonacci.llang")
```

```python
read(n);
a = 0;
b = 1;
c = 1;
if(n==0){
    write(a);
} else {
    if (n==1) {
        write(b);
    } else {
        i = 2;
        while (i <= n) {
            c = a + b;
            a = b;
            b = c;
            i = i+1;
        };
        write(c);
    };
};
```

Плюсы:

* Написал достаточно удобные хелпер для запуска

Минусы:

* Небольшие баги в парсинге
* Markdown грамматика имеет неправильный синтаксис, поэтому не рендерится)

### Язык LLang [Дима(DimaOrekhov)](https://github.com/DimaOrekhov/fl_2020_ifmo_spr)

Синтаксис:

* Какой-то неконсистентный синтаксис.

Принимается
```python
read x;
```
Не принимается
```python
write x;
```

* `factorial.llang` Печатает факториал заданного числа

```
fac = 1;
i = 1;
read number;
while (i <= number) {
    fac = fac * i;
    i = i+1;
}
write (fac);
```

* `fibonacci.llang` Печатает заданное число Фибоначчи 

```python
read n;
a = 0;
b = 1;
c = 1;
if (n == 0) then {
    write(a);
} else {
    if ( n == 1) then {
        write(b);
    } else {
        i = 2;
        while (i <= n) {
            c = a + b;
            a = b;
            b = c;
            i = i + 1;
        }
        write(c);
    }
}
```

С/Python-подобный синтаксис, хотя `then` как-то выделяется из общей картины.
Багов не найдено.
