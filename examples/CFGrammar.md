### Описание грамматики

Синтаксис похож на BNF. Правила разделены `;`. Пробелы игнорируются.
Пустая последовательность кодируется отдельным нетерминалом `<eps> ::= '';`.
Нетерминалы обозначаются идентификатором, обернутым в `<>`.
Терминалы оборачиваются в одинарные кавычки.
Каждое правило состоит из последовательностей(в коде именуемая `RValue`), разделенных символом `|`(к коде именуемая `Alternative`). Каждая такая послеловательность  состоит из терминалов и нетерминалов(к коде именуемые `Symbol.Terminal` и `Symbol.NonTerminal`).

КС-грамматика для языка описания грамматик.

```cpp
grammar ::= sequence
sequence ::= (rule ';')*
rule ::= nonterminal '::=' rvalue
rvalue ::= alternatives
alternatives ::= alternative ('|' alternative)*
alternative ::= symbol+
symbol ::= terminal | nonterminal;
nonterminal ::= '<'identifier'>'
identifier ::= [a-zA-Z][a-zA-Z0-9'_- ]*
terminal ::= '\'~[\r\n']*\''
```
