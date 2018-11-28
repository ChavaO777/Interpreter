# Interpreter
Interpreter project for the Compiler Design class

## Instructions for compiling

Execute the following commands in bash:

### a. Debug mode

```
Make CFLAGS="-D_PRINT_SYNTAX_TREE -D_PRINT_SYMBOL_TABLE"
```

> **Note:** This would print both the syntax tree and the symbol table of the input program. If desired, only one of those flags could be passed so either the syntax tree or the symbol table is printed.

### b. Standard mode

```
Make
```

> **Note:** In the Makefile, the "-ll" flag is "-lfl" in Windows. The "-t" flag is the trace flag. See the file for details.

## Instructions for running a test

- Compile the project
- Execute the following command in bash:
```
./a.out <PATH_TO_TEST_NAME>
```

**E.g.** 

```
./a.out test/print/integers/test.print_expr.oliart
```

**Note:** There's a pending bug that has to do with the parsing of the forward slash ('/') character. We're working on that fix.