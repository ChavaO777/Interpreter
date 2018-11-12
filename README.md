# Interpreter
Interpreter project for the Compiler Design class

# Instructions for compiling and running

- flex <LEX_FILE_NAME.lex>
- bison -d [-t] <Y_FILE_NAME.y>
- gcc lex.yy.c <TAB.C_FILE_NAME> -ll
- ./a.out

> **Note:** the "-ll" flag is "-lfl" in Windows. The "-t" flag is the trace flag.

**E.g.**

flex interpreter.lex
bison -d interpreter.y
gcc lex.yy.c interpreter.tab.c -ll
./a.out

# Instructions for running a test

- Compile the project
- ./a.out <PATH_TO_TEST_NAME>

**E.g.** 

./a.out test/print/integers/test.print_expr.oliart