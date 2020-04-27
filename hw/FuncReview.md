### Язык LLang [Лизы(Liza858)](https://github.com/Liza858/fl_2020_ifmo_spr)

Замечания:

* нельзя ставить пробелы в выражениях

* странное ограничение, что функция `main` всегда первая

* странное ограничение, нельзя записать стейтменты вне тела функции

* почему-то не поддерживаются функции без аргументов

Запускать:

 `runParser parseProg <$> (readFile "liza_prog.llang")`


```python
def main(arg) {
    read(number);
    write(factorial(n));
    write(factorial_rec(n));
    write(fibonacci(n));
    write(const(1,2));
};

def factorial(n) { 
    fac = 1; 
    i = 1; 
    while (i<=n) { 
        fac = fac*i;
        i = i+1; 
    };
    return fac; 
};

def factorial_rec(n) {
    if (n==0) {
        return 1;
    } else {
        return n*factorial_rec(n-1); 
    };
};

def fibonacci(n) {
    a = 0;
    b = 1;
    c = 1;
    if(n==0){
        return a;
    } else {
        if (n==1) {
            return b;
        } else {
            i = 2;
            while (i<=n) {
                c = a+b;
                a = b;
                b = c;
                i = i+1;
            };
            return c;
        };
    };
};


def const(x,y) {
    return x;
};
```

### Язык LLang [Димы(DimaOrekhov)](https://github.com/DimaOrekhov/fl_2020_ifmo_spr)

Замечания:

* надо оборачивать в скобочки выражение, которое возвращается из функции

```python
def factorial(n) {
    fac = 1;
    i = 1;
    while (i <= n) {
        fac = fac * i;
        i = i+1;
    }
    return (fac);
}

def factorial_rec(n) {
    if (n == 0) then {
        return (1);
    } else {
        return (n*factorial_rec(n-1)); 
    }
}

def fibonacci(n) {
    read(n);
    a = 0;
    b = 1;
    c = 1;
    if (n == 0) then {
        return (a);
    } else {
        if ( n == 1) then {
            return (b);
        } else {
            i = 2;
            while (i <= n) {
                c = a + b;
                a = b;
                b = c;
                i = i + 1;
            }
            return (c);
        }
    }
}


def foo() {
    return (42);
}

def const(x,y) {
    return (foo());
}

def main() {
    read(number);
    write(factorial(n));
    write(factorial_rec(n));
    write(fibonacci(n));
    write(const(1,2));
}
```
