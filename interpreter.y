/**
 * This file contains a syntax recognizer for a given grammar.
 * 
 * @Author: Guillermo Garduño García - A01322809
 * @Author: Salvador Orozco Villalever - A07104218
 * @Version: 10/31/2018
 */

/**
 * The required includes
 */

%{
#include<stdio.h>
#include<math.h>
%}

/**
 * The grammar's terminals. The declaration below turns inot the constants definition
 * in the file interpreter.tab.h that must be included in the .flex file.
 */ 

// %token NUM SUMA RESTA SYMBOL_MODULO DIVIDE MULTI SYMBOL_EXPONENT PAREND PARENI FINEXP
// %start exp

%token RES_WORD_PROGRAM IDENTIFIER SYMBOL_LT_BRACKET SYMBOL_RT_BRACKET SYMBOL_SEMICOLON RES_WORD_VAR SYMBOL_COLON INTEGER_NUMBER FLOATING_POINT_NUMBER RES_WORD_SET RES_WORD_READ RES_WORD_PRINT RES_WORD_IF SYMBOL_LT_PARENTHESES SYMBOL_RT_PARENTHESES RES_WORD_IFELSE RES_WORD_WHILE RES_WORD_FOR RES_WORD_TO RES_WORD_STEP RES_WORD_DO SYMBOL_PLUS SYMBOL_MINUS SYMBOL_STAR SYMBOL_FORWARD_SLASH SYMBOL_LT SYMBOL_GT SYMBOL_EQ SYMBOL_LEQ SYMBOL_GEQ
%start prog 

%%

// exp : expr FINEXP    { printf("Valor: %d\n", $1); }
// ;

// expr : expr SUMA term             { $$ = $1 + $3; } 
//      | expr RESTA term            { $$ = $1 + $3; }
//      | expr SYMBOL_MODULO term    { $$ = $1 % $3; } 
//      | term                       { $$ = $1; }
// ;

// factor : PARENI expr PAREND  { $$ = $2; }
//        | NUM                 { $$ = $1; }
// ; 

// term : term MULTI factor              { $$ = $1 * $3; } 
//      | term DIVIDE factor             { $$ = $1 / $3; } 
//      | term SYMBOL_EXPONENT factor    { $$ = pow($1, $3); }
//      | factor                         { $$ = $1; } 
// ;

prog : RES_WORD_PROGRAM IDENTIFIER SYMBOL_LT_BRACKET opt_decls SYMBOL_RT_BRACKET stmt
;

opt_decls : decls
;

decls : dec SYMBOL_SEMICOLON decls 
      | dec
;

dec : RES_WORD_VAR IDENTIFIER SYMBOL_COLON tipo
;

tipo : INTEGER_NUMBER
    | FLOATING_POINT_NUMBER
;

stmt : assign_stmt
     | if_stmt
     | iter_stmt
     | cmp_stmt
;

assign_stmt : RES_WORD_SET IDENTIFIER expr SYMBOL_SEMICOLON
           | RES_WORD_READ IDENTIFIER SYMBOL_SEMICOLON
           | RES_WORD_PRINT expr SYMBOL_SEMICOLON
;

if_stmt : RES_WORD_IF SYMBOL_LT_PARENTHESES expresion SYMBOL_RT_PARENTHESES stmt
        | RES_WORD_IFELSE SYMBOL_LT_PARENTHESES expresion SYMBOL_RT_PARENTHESES stmt stmt
;

iter_stmt : RES_WORD_WHILE SYMBOL_LT_PARENTHESES expresion SYMBOL_RT_PARENTHESES stmt
          | RES_WORD_FOR RES_WORD_SET IDENTIFIER RES_WORD_TO expr RES_WORD_STEP expr RES_WORD_DO stmt
;

cmp_stmt : SYMBOL_LT_BRACKET SYMBOL_RT_BRACKET
         | SYMBOL_LT_BRACKET stmt_lst SYMBOL_RT_BRACKET
;

stmt_lst : stmt | stmt_lst stmt
;

expr : expr SYMBOL_PLUS term
     | expr SYMBOL_MINUS term
     | term
;

term : term SYMBOL_STAR factor
     | term SYMBOL_FORWARD_SLASH factor
     | factor
;

factor : SYMBOL_LT_PARENTHESES expr SYMBOL_RT_PARENTHESES
       | IDENTIFIER
       | INTEGER_NUMBER
       | FLOATING_POINT_NUMBER
;

expresion : expr SYMBOL_LT expr
          | expr SYMBOL_GT expr
          | expr SYMBOL_EQ expr
          | expr SYMBOL_LEQ expr
          | expr SYMBOL_GEQ expr
;

%%

int yyerror(char const * s) {
  fprintf(stderr, "%s\n", s);
}

int main() {

  yyparse();
  return 0;
}

