FLEX=flex
BISON=bison
CC=gcc

PROGRAM = interpreter
LEX = interpreter.lex
PARSER = interpreter.y

$(PROGRAM): $(LEX) $(PARSER)
	$(FLEX) $(LEX)
	$(BISON) -d -t $(PARSER)
	$(CC) $(CFLAGS) *.c -ll