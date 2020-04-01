

## **L** is for stripped down [Lua](http://lua-users.org/wiki/LuaFourOneGrammar)

```
digit ::= '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0'

num ::= digit | digit num

alphabet ::= [A-Za-z_]
tick ::= '\''
chars ::= alphabet | alphabet chars

string = '\"' chars '\"'

var ::= chars | var tick

reserved ::= 'let'
           | 'do' | 'done'
           | 'for'
           | 'while'
           | 'function'
           | 'if'
           | 'return'
           | 'nil'
           | 'true'
           | 'false'
           | 'pass'
           | biop
           | unop
           | '('
           | ')'
           | ';'
           | '|'

biop ::= '+'  |  '-'  |  '*'  |  '/'  |  '^'  |  '%'  | '<' | '<=' | '>' | '>=' | '==' | '\=' | and | or
unop ::= '-' | 'not' 

exp ::= 'nil' | 'false' | 'true' | num | exp biop exp | unop exp | var | '(' exp ')' | functioncall | string

block ::= stat | stat ';' | stat ';' block

stat ::= let var '=' exp
       | var '=' exp
       | 'for' '(' block '|' exp '|' block ')' 'do' block 'done' 
       | 'while' '(' exp ')' 'do' block 'done' 
       | 'if' '(' exp ')' 'do' block 'done' 
       | 'let' 'function' funcname funcbody
       | functioncall
       | 'return' exp

funcname ::= var 
funcbody ::= '(' params ')' 'do' block 'done'
params ::= var | var ',' params

exps = exp | exp ',' exps

functioncall ::= funcname '(' exps ')'
```
Внутри скобок, пробелы между скобкой и выражением опциональны.

### Следует заметить:
    1. Опциональность ';' в последнем statement'е block'а; 
    2. Отсутствие 'else' в if;
    3. Все функции хотя бы от одного аргумента.

Examples:

```
    let some_variable = 12;
    let some_variable' = some_variable;
    some_variable = 13;


    let greeting = "hello";
    let function print_greeting(ignored) do
        print(greeting)
    done;

    print_greeting(nil);

    let function fib_exp(n) do
        if (n <= 1) do
            return n;
        done; 
        return fib_exp(n-1) + fib_exp(n-2)
    done

    let function fib(n) do
        let fib' = 0;
        let fib'' = 1;
        let fib''' = 0;
        if (n == 0) do
            return fib'
        done;

        for (let i = 0 | i <= n | i = i + 1) do
            fib''' = fib' + fib'';
            fib' = fib'';
            fib'' = fib''';
        done;
        
        return fib''
    done;

```

Следующий пример не несёт никакой смысловой нагрузки, помимо демонстрации корректной, с точки зрения 
выше описанной грамматики, функции:
```lua
    let function nonsense(ignored) do
        inside(hello_nonsense);

        let function inside(a, b, c, d, e, f, g) do
            for (let h = 12;
                 let function hello_nonsense(print_hello) do 
                     for (print_hello(a) | print_hello(b) | print_hello(c)) do
                        print_hello(d)
                     done;
                     return print_hello
                 done |

                 print_hello(nonsense) | 

                 while (nonsense(ignored)) do
                    ignored(true);
                 done;)
            do
                return hello_nonsense;
            done;
        done;
    done;
```

1. Разделять ли операторы языка разделителями или сделать значимыми переносы строк и отсупы?

   Отступы не значимы.
   Везде, где разрешены пробелы, они могут быть помножены на неограниченное количество.

---

2. Использовать скобки для группировки блоков кода, ключевые слова begin/end или маркеры конца блока, зависящие от того, в контексте какого оператора мы находимся?

   Ключевые слова do и done для всех блоков statement'ов; скобки для группировки выражений, условий while блоков, инициализирующих statement'ов и выражений цикла for и параметров и аргументов функций.
   Подобный код является корректным с точки зрения грамматики:
```lua
    let function func(var) do
        while (((((1-2))))) do
            func(var)
        done  
    done;

    func((((((((true))))))));
``` 
---

3. Может вы хотите предоставить синтаксический сахар для облегчения синтаксиса? 

   Синтаксический сахар пока не предусмотрен.

---

4. Как в вашем языке выглядит целое число? `---1`, `+13` и `+-0` -- корректные числа или нет?

 ```
digit ::= '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0'

num ::= digit | digit num
```

   Все упомянутые числа являются корректными, за исключением `+-0` из-за отсутствия унарного плюса.

---

5. Как в вашем языке выглядит идентификатор? Можно ли в идентификаторах использовать какие-нибудь специальные символы?

   Идентификатор не может начинаться с цифры.
   Кроме больших и маленьких букв латинского алфавита в теле идентификатора разрешено нижнее подчеркивание '\_' в любой части идентификатора и неограниченное количество апострофов '\'' в его конце.

---

6. Как выглядят ключевые слова?

Cм. ```reserved```.
