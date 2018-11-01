# Interpreter
Interpreter project for the Compiler Design class

# Instructions for compiling and running

flex <LEX_FILE_NAME.lex>
bison -d <Y_FILE_NAME.y>
gcc lex.yy.c <TAB.C_FILE_NAME> -ll
./a.out

E.g.

flex calculadora.lex
bison -d calculadora.y
gcc lex.yy.c calculadora.tab.c -ll
./a.out

Note: the "-ll" flag is "-lfl" in Windows.