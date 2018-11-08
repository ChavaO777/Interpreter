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
#include <stdio.h>
#include <math.h>
#include "interpreter.tab.h"

#include <stdlib.h> /* malloc. */
#include <string.h> /* strlen. */

#define NOTHING -99999

// Enum for identifying the parent node of each node in the syntax tree.
// This enum must be in sync with the SyntaxTreeNodeTypeName array.
enum SyntaxTreeNodeType { 
  PROGRAM, 
  STMT, 
  ASSIGN_STMT, 
  IF_STMT, 
  ITER_STMT, 
  CMP_STMT, 
  SET, 
  EXPR, 
  TERM, 
  FACTOR, 
  READ, 
  PRINT, 
  IF, 
  EXPRESION, 
  IFELSE, 
  WHILE, 
  FOR, 
  STEP, 
  DO, 
  STMT_LST, 
  PLUS,
  MINUS, 
  STAR, 
  FORWARD_SLASH, 
  LT, 
  GT, 
  EQ, 
  LEQ, 
  GEQ, 
  INTEGER_NUMBER_VALUE, 
  FLOATING_POINT_NUMBER_VALUE, 
  ID_VALUE
};

// Array of the names of the syntax tree node types.
// This array must be in sync with the SyntaxTreeNodeType enum.
char* SyntaxTreeNodeTypeName[] = { 
  "PROGRAM", 
  "STMT", 
  "ASSIGN_STMT", 
  "IF_STMT", 
  "ITER_STMT", 
  "CMP_STMT", 
  "SET", 
  "EXPR", 
  "TERM", 
  "FACTOR", 
  "READ", 
  "PRINT", 
  "IF", 
  "EXPRESION", 
  "IFELSE", 
  "WHILE", 
  "FOR", 
  "STEP", 
  "DO", 
  "STMT_LST", 
  "PLUS",
  "MINUS", 
  "STAR", 
  "FORWARD_SLASH", 
  "LT", 
  "GT", 
  "EQ", 
  "LEQ", 
  "GEQ", 
  "INTEGER_NUMBER_VALUE", 
  "FLOATING_POINT_NUMBER_VALUE", 
  "ID_VALUE"
};

// Declaration of the createNode function.
struct SyntaxTreeNode* createNode(
  int, 
  double, 
  char*,
  int, 
  int,
  struct SyntaxTreeNode*, 
  struct SyntaxTreeNode*, 
  struct SyntaxTreeNode*, 
  struct SyntaxTreeNode*, 
  struct SyntaxTreeNode*);

// Declaration of the printTree function.
void printTree(struct SyntaxTreeNode*);
%}

/**
 * The grammar's terminals. The declaration below turns into the constants definition
 * in the file interpreter.tab.h that must be included in the .flex file.
 */ 

// Start token
%start prog 

%union {

  int intVal;
  double doubleVal;
  char* idName;
  struct SyntaxTreeNode* treeVal;
}

// Token declarations
%token RES_WORD_PROGRAM
%token IDENTIFIER
%token SYMBOL_LT_BRACKET
%token SYMBOL_RT_BRACKET
%token SYMBOL_SEMICOLON
%token RES_WORD_VAR
%token SYMBOL_COLON 
%token RES_WORD_INT
%token RES_WORD_FLOAT
%token INTEGER_NUMBER
%token FLOATING_POINT_NUMBER
%token RES_WORD_SET
%token RES_WORD_READ
%token RES_WORD_PRINT 
%token RES_WORD_IF
%token SYMBOL_LT_PARENTHESES
%token SYMBOL_RT_PARENTHESES
%token RES_WORD_IFELSE
%token RES_WORD_WHILE
%token RES_WORD_FOR
%token RES_WORD_TO 
%token RES_WORD_STEP
%token RES_WORD_DO
%token SYMBOL_PLUS
%token SYMBOL_MINUS
%token SYMBOL_STAR
%token SYMBOL_FORWARD_SLASH
%token SYMBOL_LT
%token SYMBOL_GT
%token SYMBOL_EQ 
%token SYMBOL_LEQ
%token SYMBOL_GEQ

// Types
%type <treeVal> prog
%type <treeVal> stmt
%type <treeVal> assign_stmt
%type <treeVal> if_stmt
%type <treeVal> iter_stmt
%type <treeVal> cmp_stmt
%type <treeVal> stmt_lst
%type <treeVal> expr
%type <treeVal> term
%type <treeVal> factor
%type <treeVal> expresion
%type <treeVal> SYMBOL_LT_BRACKET
%type <treeVal> SYMBOL_RT_BRACKET 
%type <treeVal> RES_WORD_PROGRAM
%type <treeVal> RES_WORD_VAR
%type <treeVal> RES_WORD_SET
%type <treeVal> RES_WORD_READ
%type <treeVal> RES_WORD_PRINT
%type <treeVal> RES_WORD_IF
%type <treeVal> RES_WORD_IFELSE 
%type <treeVal> RES_WORD_WHILE
%type <treeVal> RES_WORD_FOR
%type <treeVal> RES_WORD_TO
%type <treeVal> RES_WORD_STEP
%type <treeVal> RES_WORD_DO
%type <treeVal> SYMBOL_PLUS
%type <treeVal> SYMBOL_MINUS
%type <treeVal> SYMBOL_STAR 
%type <treeVal> SYMBOL_FORWARD_SLASH
%type <treeVal> SYMBOL_LT
%type <treeVal> SYMBOL_GT
%type <treeVal> SYMBOL_EQ
%type <treeVal> SYMBOL_LEQ
%type <treeVal> SYMBOL_GEQ
%type <treeVal> INTEGER_NUMBER
%type <treeVal> FLOATING_POINT_NUMBER
%type <treeVal> IDENTIFIER
%type <treeVal> SYMBOL_LT_PARENTHESES
%type <treeVal> SYMBOL_RT_PARENTHESES
//%type <intVal> INTEGER_NUMBER
//%type <doubleVal> FLOATING_POINT_NUMBER
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
      {
        struct SyntaxTreeNode* syntaxTreeRoot;
        syntaxTreeRoot = createNode(NOTHING, NOTHING, NULL, PROGRAM, NOTHING, $6, NULL, NULL, NULL, NULL);
        printTree(syntaxTreeRoot);
      }
;

// ########### START of the rules for DECLARATIONS OF VARIABLES ###########
// Declarations are not part of the syntax tree node
opt_decls : decls
          | /* epsilon */
;

decls : dec SYMBOL_SEMICOLON decls 
      | dec
;

dec : RES_WORD_VAR IDENTIFIER SYMBOL_COLON tipo
;

tipo : RES_WORD_INT
     | RES_WORD_FLOAT
;

// ########### END of the rules for DECLARATIONS OF VARIABLES ###########

stmt : assign_stmt { $$ = $1; }
     | if_stmt { $$ = $1; }
     | iter_stmt { $$ = $1; }
     | cmp_stmt { $$ = $1; }
;

assign_stmt : RES_WORD_SET IDENTIFIER expr SYMBOL_SEMICOLON
            {
              struct SyntaxTreeNode* idNode = createNode(NOTHING, NOTHING, (char *)$2, ID_VALUE, SET, NULL, NULL, NULL, NULL, NULL);
              $$ = createNode(NOTHING, NOTHING, NULL, SET, STMT, idNode, $3, NULL, NULL, NULL);
            }
            | RES_WORD_READ IDENTIFIER SYMBOL_SEMICOLON
            {
              struct SyntaxTreeNode* idNode = createNode(NOTHING, NOTHING, (char *)$2, ID_VALUE, READ, NULL, NULL, NULL, NULL, NULL);
              $$ = createNode(NOTHING, NOTHING, NULL, READ, STMT, idNode, NULL, NULL, NULL, NULL);              
            }
            | RES_WORD_PRINT expr SYMBOL_SEMICOLON
            {
              $$ = createNode(NOTHING, NOTHING, NULL, PRINT, STMT, $2, NULL, NULL, NULL, NULL);
            }
;

if_stmt : RES_WORD_IF SYMBOL_LT_PARENTHESES expresion SYMBOL_RT_PARENTHESES stmt
        {
          $$ = createNode(NOTHING, NOTHING, NULL, IF, IF_STMT, $3, $5, NULL, NULL, NULL);
        }
        | RES_WORD_IFELSE SYMBOL_LT_PARENTHESES expresion SYMBOL_RT_PARENTHESES stmt stmt
        {
          $$ = createNode(NOTHING, NOTHING, NULL, IFELSE, IF_STMT, $3, $5, $6, NULL, NULL);
        }
;

iter_stmt : RES_WORD_WHILE SYMBOL_LT_PARENTHESES expresion SYMBOL_RT_PARENTHESES stmt
          {
            $$ = createNode(NOTHING, NOTHING, NULL, WHILE, ITER_STMT, $3, $5, NULL, NULL, NULL);
          }
          | RES_WORD_FOR RES_WORD_SET IDENTIFIER expr RES_WORD_TO expr RES_WORD_STEP expr RES_WORD_DO stmt
;

cmp_stmt : SYMBOL_LT_BRACKET SYMBOL_RT_BRACKET { $$ = NULL; }
         | SYMBOL_LT_BRACKET stmt_lst SYMBOL_RT_BRACKET { $$ = $2; }
;

stmt_lst : stmt { $$ = $1;}
         | stmt_lst stmt 
         {
            $$ = createNode(NOTHING, NOTHING, NULL, STMT_LST, STMT_LST, $1, $2, NULL, NULL, NULL);
         }
;

expr : expr SYMBOL_PLUS term 
     {
        $$ = createNode(NOTHING, NOTHING, NULL, PLUS, EXPR, $1, $3, NULL, NULL, NULL);
     }
     | expr SYMBOL_MINUS term
     {
        $$ = createNode(NOTHING, NOTHING, NULL, MINUS, EXPR, $1, $3, NULL, NULL, NULL);
     }
     | term { $$ = $1; }
;

term : term SYMBOL_STAR factor
     {
        $$ = createNode(NOTHING, NOTHING, NULL, STAR, EXPR, $1, $3, NULL, NULL, NULL);
     }
     | term SYMBOL_FORWARD_SLASH factor
     {
        $$ = createNode(NOTHING, NOTHING, NULL, FORWARD_SLASH, EXPR, $1, $3, NULL, NULL, NULL);
     }
     | factor { $$ = $1; }
;

factor : SYMBOL_LT_PARENTHESES expr SYMBOL_RT_PARENTHESES
       {
          $$ = $2;
       }
       | IDENTIFIER
       {
          $$ = createNode(NOTHING, NOTHING, (char *)$1, ID_VALUE, FACTOR, NULL, NULL, NULL, NULL, NULL);
       }
       | INTEGER_NUMBER
       {
          //printf("$1 =%p\n",$1);
          //printf("value of $1 = %d\n",(int)$1);
          //$1 stores integer number as a ponter we should cast it before creating node.
          $$ = createNode((int)$1, NOTHING, NULL, INTEGER_NUMBER_VALUE, TERM, NULL, NULL, NULL, NULL, NULL);
       }
       | FLOATING_POINT_NUMBER
;

expresion : expr SYMBOL_LT expr
          {
            $$ = createNode(NOTHING, NOTHING, NULL, LT, EXPRESION, $1, $3, NULL, NULL, NULL);
          }
          | expr SYMBOL_GT expr
          {
            $$ = createNode(NOTHING, NOTHING, NULL, GT, EXPRESION, $1, $3, NULL, NULL, NULL);
          }
          | expr SYMBOL_EQ expr
          {
            $$ = createNode(NOTHING, NOTHING, NULL, EQ, EXPRESION, $1, $3, NULL, NULL, NULL);
          }
          | expr SYMBOL_LEQ expr
          {
            $$ = createNode(NOTHING, NOTHING, NULL, LEQ, EXPRESION, $1, $3, NULL, NULL, NULL);
          }
          | expr SYMBOL_GEQ expr
          {
            $$ = createNode(NOTHING, NOTHING, NULL, GEQ, EXPRESION, $1, $3, NULL, NULL, NULL);
          }
;

%%

// Error codes
#define ERROR_CODE_SYMBOL_NOT_FOUND                                         1
#define ERROR_CODE_INVALID_ASSIGNMENT_TO_INT_SYMBOL                         2
#define ERROR_CODE_INVALID_ASSIGNMENT_TO_FLOATING_POINT_SYMBOL              3

// Error messages
#define ERROR_MESSAGE_SYMBOL_NOT_FOUND                                      "Attempted to retrieve a non-existent symbol."
#define ERROR_MESSAGE_INVALID_ASSIGNMENT_TO_INT_SYMBOL                      "Attempted to assign an integer value to a symbol storing a floating-point value."
#define ERROR_MESSAGE_INVALID_ASSIGNMENT_TO_FLOATING_POINT_SYMBOL           "Attempted to assign a floating-point value to a symbol storing an integer value."

// Symbol table data types
#define SYMBOL_TABLE_NODE_INTEGER_DATA_TYPE                                 1
#define SYMBOL_TABLE_NODE_FLOATING_POINT_DATA_TYPE                          2

void handleError(int errorCode, char *errorMessage){

  printf("Error #%d: %s\n", errorCode, errorMessage);
  exit(1);
}

/* Data type for the nodes in the symbol table */

struct SymbolTableNode {

  char *name;

  // This attribute represents the data type stored by this symbol: 
  // int, float or pointer to function (for later).
  int type;
  union {

    int intVal; /* Integer value */
    double doubleVal; /* Floating-point value */
    // struct node* ptrFunction; // For later
  } value;

  struct SymbolTableNode *next;
};

// Declaration of the head of the linked list representing the symbol table.
struct SymbolTableNode *symbolTableHead;

/**
 * Function that inserts a new symbol to the symbol table.
 * 
 * @param symbolName the name of the symbol to insert.
 * @param symbolType the type of the symbol to insert: int, float or function pointer
 * @returns a pointer to the new node, which will now be the head of the linked list that 
 * represents the symbol table.
 */ 
struct SymbolTableNode* insertToSymbolTable(char const *symbolName, int symbolType){

  // Malloc for the new node
  struct SymbolTableNode* newNodePtr = (struct SymbolTableNode*) malloc(sizeof(struct SymbolTableNode));
  
  // Malloc for the symbol's name
  newNodePtr->name = (char *) malloc(strlen(symbolName) + 1);
  strcpy (newNodePtr->name, symbolName);
  newNodePtr->type = symbolType;

  // Set the default values to 0
  newNodePtr->value.intVal = 0;
  newNodePtr->value.doubleVal = 0.0;

  // Insert at the beginning of the list
  newNodePtr->next = (struct SymbolTableNode*)symbolTableHead;

  return newNodePtr;
}

/**
 * Function that retrieves a symbol from the symbol table given its name.
 * 
 * @param symbolName the name of the symbol to retrieve.
 * @returns a pointer to the symbol or 0 if the symbol was not found.
 */ 
struct SymbolTableNode* retrieveFromSymbolTable(char const *symbolName){

  struct SymbolTableNode *currPtr = symbolTableHead;

  // Traverse the linked list and compare the name of the current symbol
  // with the name of the symbol that you're looking for.
  while(currPtr != NULL){

    if(strcmp(currPtr->name, symbolName) == 0)
      return currPtr;

    currPtr = currPtr->next;
  }

  handleError(ERROR_CODE_SYMBOL_NOT_FOUND, ERROR_MESSAGE_SYMBOL_NOT_FOUND);
  return 0;
}

/**
 * Function that assigns an integer value to a symbol in the symbol table.
 * 
 * @param symbolName the name of the symbol to which a value will be assigned.
 * @param newIntegerValue the new integer value to assign to that symbol.
 */ 
void setIntValueToSymbol(char const *symbolName, int newIntegerValue){

  struct SymbolTableNode *symbolPtr = retrieveFromSymbolTable(symbolName);

  if(symbolPtr != NULL){

    // Check that the symbol does in fact store an integer
    if(symbolPtr->type == SYMBOL_TABLE_NODE_INTEGER_DATA_TYPE){

      symbolPtr->value.intVal = newIntegerValue;
    }
    else{

      // Error out and exit!
      handleError(ERROR_CODE_INVALID_ASSIGNMENT_TO_INT_SYMBOL, 
        ERROR_MESSAGE_INVALID_ASSIGNMENT_TO_INT_SYMBOL);
    }
  }
}

/**
 * Function that assigns a floating-point value to a symbol in the symbol table.
 * 
 * @param symbolName the name of the symbol to which a value will be assigned.
 * @param newDoubleValue the new floating-point value to assign to that symbol.
 */ 
void setDoubleValueToSymbol(char const *symbolName, double newDoubleValue){

  struct SymbolTableNode *symbolPtr = retrieveFromSymbolTable(symbolName);

  if(symbolPtr != NULL){

    // Check that the symbol does in fact store a double
    if(symbolPtr->type == SYMBOL_TABLE_NODE_FLOATING_POINT_DATA_TYPE){

      symbolPtr->value.doubleVal = newDoubleValue;
    }
    else{

      // Error out and exit!
      handleError(ERROR_CODE_INVALID_ASSIGNMENT_TO_FLOATING_POINT_SYMBOL, 
        ERROR_MESSAGE_INVALID_ASSIGNMENT_TO_FLOATING_POINT_SYMBOL);
    }
  }
}

/**
 * Function that retrieves the integer value of a symbol in the symbol table.
 * 
 * @param symbolName the name of the symbol to which a value will be assigned.
 * @returns the integer value stored by that symbol
 */ 
int getIntValueFromSymbol(char const *symbolName){

  struct SymbolTableNode *symbolPtr = retrieveFromSymbolTable(symbolName);

  if(symbolPtr != NULL){

    return symbolPtr->value.intVal;
  }

  return 0;
}

/**
 * Function that retrieves the floating-point value of a symbol in the symbol table.
 * 
 * @param symbolName the name of the symbol to which a value will be assigned.
 * @returns the floating-point value stored by that symbol
 */ 
int getFloatingPointValueFromSymbol(char const *symbolName){

  struct SymbolTableNode *symbolPtr = retrieveFromSymbolTable(symbolName);

  if(symbolPtr != NULL){

    return symbolPtr->value.doubleVal;
  }

  return 0;
}

/**
 * Structure of the node for the syntax tree.
 */ 
struct SyntaxTreeNode {

  // This attribute represents the data type stored by this node.
  int type;
  int parentNodeType;
  struct SyntaxTreeNode *arrPtr[4];
  union {
    int intVal; /* Integer value */
    double doubleVal; /* Floating-point value */
    char *idName; /* Identifier name */
  } value;
  struct SyntaxTreeNode *next;
};

/**
 * Function for creating a node in the syntax tree.
 * 
 * @param iVal the value of the node in case it represents an integer number
 * @param dVal the value of the node in case it represents an floating-point number
 * @param idName the value of the node in case it represents an identifier
 * @param type the type of the node, an entry of the SyntaxTreeNodeType enum.
 * @param parentNodeType the type of the parent node, an entry of the SyntaxTreeNodeType enum.
 * @param ptr1 a pointer to the 1st child node.
 * @param ptr2 a pointer to the 2nd child node.
 * @param ptr3 a pointer to the 3rd child node.
 * @param ptr4 a pointer to the 4th child node.
 * @param nextNode a pointer to the next node.
 * @return A pointer to the node just created.
 */ 
struct SyntaxTreeNode* createNode(int iVal, double dVal, char* idName,
  int type, int parentNodeType,
  struct SyntaxTreeNode* ptr1, struct SyntaxTreeNode* ptr2, 
  struct SyntaxTreeNode* ptr3, struct SyntaxTreeNode* ptr4, struct SyntaxTreeNode* nextNode){

    struct SyntaxTreeNode* newNodePtr = (struct SyntaxTreeNode*) malloc(sizeof(struct SyntaxTreeNode));
    newNodePtr->type = type;
    newNodePtr->parentNodeType = parentNodeType;

    newNodePtr->arrPtr[0] = ptr1;
    newNodePtr->arrPtr[1] = ptr2;
    newNodePtr->arrPtr[2] = ptr3;
    newNodePtr->arrPtr[3] = ptr4;

    newNodePtr->next = nextNode;
	
    //printf("%p\n",ptr1);
    // Assign the values. They could be equal to NOTHING.
    if(type == INTEGER_NUMBER_VALUE){

      newNodePtr->value.intVal = iVal;
    }
    else if(type == FLOATING_POINT_NUMBER_VALUE){

      newNodePtr->value.doubleVal = dVal;
    }
    else if(type == ID_VALUE){

      // Malloc for the identifier's name
      newNodePtr->value.idName = (char *) malloc(strlen(idName) + 1);
      strcpy (newNodePtr->value.idName, idName);
    }

    return newNodePtr;
}

void printNodeType(int type, char* name){

  // If our names array contains an entry for this type
  if(type >= 0 && type < sizeof(SyntaxTreeNodeTypeName)){

    printf("%s: %s\n", name, SyntaxTreeNodeTypeName[type]);
  }
  else{

    printf("%s: %d\n", name, type);
  }
}

void printTree(struct SyntaxTreeNode* node){

  if(node == NULL)
    return;

  printNodeType(node->type, "type");
  printf("address = %p\n", node);
  printNodeType(node->parentNodeType, "parentNodeType");

  if(node->type == INTEGER_NUMBER_VALUE)
    printf("Node value = %d\n",node->value.intVal);

  else if(node->type == ID_VALUE){

    printf("Node value = %s\n",node->value.idName);
  }

  int i = 0;
  for(i = 0; i < 4; i++)
    printf("ptr #%d: %p\n", i + 1, node->arrPtr[i]);

  printf("\n");

  for(i = 0; i < 4; i++)
    printTree(node->arrPtr[i]);
}

int yyerror(char const * s) {
  fprintf(stderr, "Error: %s\n", s);
}

int main() {

  // extern int yydebug;
  // yydebug = 1;

  yyparse();
  return 0;
}
