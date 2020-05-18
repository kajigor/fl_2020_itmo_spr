# fl_2020_ifmo_spr

Используется Python 3.7.4, но, думаю, должна подойти любая версия 3.6+.

Кроме стандартной библиотеки понадобится ply (python lex yacc):

pip install ply

или

conda install -c anaconda ply

## Запуск:

python main.py -g <grammar_file> -i <input_file>

-i - опциональный аргумент, в любом случае, пока что не поддерживает проверка выводимости цепочек из грамматики.
