/**
 * Lexical recognizer
 * 
 * @Author: Guillermo Garduño García - A01322809
 * @Author: Salvador Orozco Villalever - A07104218
 * @Version: 10/31/2018
 */ 

%{

#ifdef PRINT
#define TOKEN(t) printf("Token: " #t "\n");
#else 
#define TOKEN(t) return(t);
#endif

/* Includes */
#include <stdlib.h>
#include <math.h>

/* Include the bison-generated file to have the token definitions.*/
#include "interpreter.tab.h"

#define SYMBOL_SPACE            -118
#define NEW_LINE                -119

/* End-of-file */
#define END_OF_FILE             -100
double doubleVal;

%} 

%option lex-compat
%option yylineno

DIGIT       [0-9]
DIGITWZ     [1-9]
LETTER      [a-zA-Z]

%%
for                                                     { return RES_WORD_FOR; }
to                                                      { return RES_WORD_TO; }
step                                                    { return RES_WORD_STEP; }
do                                                      { return RES_WORD_DO; }
while                                                   { return RES_WORD_WHILE; }
program                                                 { return RES_WORD_PROGRAM; }
var                                                     { return RES_WORD_VAR; }
set                                                     { return RES_WORD_SET; }
read                                                    { return RES_WORD_READ; }
print                                                   { return RES_WORD_PRINT; }
if                                                      { return RES_WORD_IF; }
ifelse                                                  { return RES_WORD_IFELSE; }
int                                                     { return RES_WORD_INT; }
float                                                   { return RES_WORD_FLOAT; }
fun                                                     { return RES_WORD_FUN; }

"="                                                     { return SYMBOL_EQ; } /* Symbols */
"<"                                                     { return SYMBOL_LT; }
">"                                                     { return SYMBOL_GT; }
";"                                                     { return SYMBOL_SEMICOLON; }
":"                                                     { return SYMBOL_COLON; }
"<="                                                    { return SYMBOL_LEQ; }
">="                                                    { return SYMBOL_GEQ; }
"+"                                                     { return SYMBOL_PLUS; }
"-"                                                     { return SYMBOL_MINUS; }
"*"                                                     { return SYMBOL_STAR; }
"/"                                                     { return SYMBOL_FORWARD_SLASH; }
"("                                                     { return SYMBOL_LT_PARENTHESES; }
")"                                                     { return SYMBOL_RT_PARENTHESES; }
"{"                                                     { return SYMBOL_LT_BRACKET; }
"}"                                                     { return SYMBOL_RT_BRACKET; }

("-")?{DIGIT}*                                          { yylval.intVal = atoi(yytext); return INTEGER_NUMBER; /* Convert the number to INTEGER_NUMBER */}

("-")?(({DIGITWZ}{DIGIT}*|"0")"."({DIGIT}*{DIGIT}))     { doubleVal = atof(yytext); return FLOATING_POINT_NUMBER; } /*Floating-point numbers */

("$"|{LETTER}|"_")("$"|{LETTER}|"_"|{DIGIT})*           { yylval.idName = strdup(yytext); return IDENTIFIER; } /* Identifiers */

<<EOF>>                                                 { return END_OF_FILE; } /* End-of-file */

[/]+.*                                                  { printf(""); } /* Comment */

" "                                                     { ; } /* If the space returns a value, a syntax error will be produced. */

"\t"                                                     { ; } /* If the tab returns a value, a syntax error will be produced. */

"\n"                                                    { yylineno++; } /* If the new line returns a value, a syntax error will be produced. */

.                                                       { return 1100; } /* Anything else */
%%
