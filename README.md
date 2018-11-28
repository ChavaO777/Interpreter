# Interpreter
Interpreter project for the Compiler Design class

# Instructions for compiling and running

Execute the following commands in bash:

- Make
- ./a.out <INPUT_FILE_NAME>

> **Note:** In the Makefile, the "-ll" flag is "-lfl" in Windows. The "-t" flag is the trace flag. See the file for details.

**E.g.**

- Make
- ./a.out my_program_source_code.txt

# Instructions for running a test

- Compile the project
- Execute the following command in bash:
    - ./a.out <PATH_TO_TEST_NAME>

**E.g.** 

- ./a.out test/print/integers/test.print_expr.oliart

**Note:** There's a pending bug that has to do with the parsing of the forward slash ('/') character. We're working on that fix.