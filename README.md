# Interpreter
Interpreter project for the Compiler Design class

## Instructions for compiling

Execute the following commands in bash:

### a. Debug mode

```
Make CFLAGS="-D_PRINT_PARSE_TRACE -D_PRINT_SYNTAX_TREE -D_PRINT_SYMBOL_TABLE -D_PRINT_CALL_STACK"
```

> **Note:** In the debug mode, besides printing the input program's output, the interpreter would also print the following:
> 
> 1. the parse trace
> 2. the syntax tree
> 3. the symbol table of the input program
> 4. the call stack for any new function call. 
> 
> If desired, only a subset of those flags can be passed.

### b. Standard mode

```
Make
```

> **Note:** In the Makefile, the "-ll" flag should be changed to "-lfl" for Windows. See the file for details.

## Instructions for running a test

1. Compile the project
2. Execute the following command in bash:
```
./a.out <PATH_TO_TEST_NAME>
```

**E.g.** 

```
Make
./a.out test/print/integers/test.print_expr.oliart
```

**Note:** There's a pending bug that has to do with the parsing of the forward slash ('/') character. We're working on that fix.
