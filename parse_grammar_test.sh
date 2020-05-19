#!/usr/bin/env bash

printf "Простая грамматика\n"
./parse_grammar.sh src/test/resources/parse/ll1table/simple.grammar
printf "Грамматика идентификатора\n"
./parse_grammar.sh src/test/resources/grammar/identifier.grammar
printf "Невалидная грамматика\n"
./parse_grammar.sh src/test/resources/grammar/invalid.grammar
