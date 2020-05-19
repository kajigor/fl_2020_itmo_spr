#!/usr/bin/env bash

echo 'Запуск написанных тестов
'
./gradlew :test

echo ' Запуск показательных примеров

'
printf "> Простой пример\n"
./run.sh src/test/resources/parse/ll1table/simple.grammar 'a'
printf "> Звезда Клини\n"
./run.sh src/test/resources/parse/ll1table/kleene.grammar 'aaaaa'
printf "> Неправильная строка для \'+\'(одно или больше значений 'a')\n"
./run.sh src/test/resources/parse/ll1table/plus.grammar ''
printf "> Правильное выражение\n"
./run.sh src/test/resources/parse/support/expression.grammar '(n+n)*(n)'
printf "> Неправильное выражение\n"
./run.sh src/test/resources/parse/support/expression.grammar '(n+n*n'
printf "> Натуральное число(могут быть нули в начале)\n"
./run.sh src/test/resources/grammar/natural.grammar '12334'
printf "> Идентификатор\n"
./run.sh src/test/resources/grammar/identifier.grammar '_the_id_'
printf "> Грамматика не LL(1) вида #1.\n"
./run.sh src/test/resources/parse/ll1table/non_ll1.grammar '((((()))))(((((())))))'
printf "> Грамматика не LL(1) вида #2.\n"
./run.sh src/test/resources/parse/support/non_ll1_2.grammar 'ad'
