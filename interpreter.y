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

#include <assert.h>

#include <stdlib.h> /* malloc. */
#include <string.h> /* strlen. */

#define NOTHING -99999

extern FILE *yyin;

double doubleVal;

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
  TO,
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
  ID_VALUE,
  FUNCTION_VALUE,
  PARAMETER_VALUE,
  EXPR_LST,
  RETURN
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
  "TO",
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
  "ID_VALUE",
  "FUNCTION_VALUE",
  "PARAMETER_VALUE",
  "EXPR_LST",
  "RETURN"
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
void startTreePrinting(struct SyntaxTreeNode*, char*);

// Declaration of the printTree function.
void printTree(struct SyntaxTreeNode*);

// Declaration of the traverseTree function.
void traverseTree(struct SyntaxTreeNode*);

// Declaration of the insertToSymbolTable function.
void insertToSymbolTable(struct SymbolTableNode**, char const *, int, int, struct SymbolTableNode*, struct SyntaxTreeNode*);

// Declaration of the head of the linked list representing the symbol table.
struct SymbolTableNode *mainFunctionSymbolTableHead;

// Declaration of the head of the linked list representing the symbol table of a SPECIFIC FUNCTION (not the main one)
struct SymbolTableNode *functionSymbolTableHead;

// Declaration of the printSymbolTable function.
void printSymbolTable(struct SymbolTableNode *, char*);

// Declaration of the top of the function call stack
struct CurrentFunction *ptrFunctionCallStackTop;

// Declaration of the popFunctionCallToStack function.
void popFunctionCallToStack();

// Declaration of the insertFunctionCallToStack function.
void insertFunctionCallToStack(struct SymbolTableNode*);

// Declaration of the func_func function.
void func_func(struct SyntaxTreeNode*);
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
  struct SymbolTableNode* symbolTableVal;
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
%token RES_WORD_FUN
%token SYMBOL_COMMA
%token RES_WORD_RETURN

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
%type <intVal> tipo
%type <treeVal> opt_exprs
%type <treeVal> expr_lst
%%

prog : RES_WORD_PROGRAM IDENTIFIER SYMBOL_LT_BRACKET opt_decls opt_fun_decls SYMBOL_RT_BRACKET stmt 
      {
        struct SyntaxTreeNode* syntaxTreeRoot;
        syntaxTreeRoot = createNode(NOTHING, NOTHING, NULL, PROGRAM, NOTHING, $7, NULL, NULL, NULL, NULL);
        
        #ifdef _PRINT_SYNTAX_TREE
        startTreePrinting(syntaxTreeRoot, "main");
        #endif
        #ifdef _PRINT_SYMBOL_TABLE
        printSymbolTable(mainFunctionSymbolTableHead, "main");
        #endif

        printf("########## START OF PROGRAM OUTPUT ##########\n\n");
        traverseTree(syntaxTreeRoot);
        printf("########## END OF PROGRAM OUTPUT ##########\n\n");
      }
;

// ########### START of the rules for DECLARATIONS OF VARIABLES OF THE MAIN FUNCTION ###########
// Declarations are not part of the syntax tree node
opt_decls : decls
          | /* epsilon */
;

decls : dec SYMBOL_SEMICOLON decls 
      | dec
;

dec : RES_WORD_VAR IDENTIFIER SYMBOL_COLON tipo
    {
      // printf("id name = %s\n", (char*)$2);
      insertToSymbolTable(&mainFunctionSymbolTableHead, (char*)$2, $4, NOTHING, NULL, NULL);
    }
;

tipo : RES_WORD_INT
     {
       $$ = INTEGER_NUMBER_VALUE;
     }
     | RES_WORD_FLOAT
     {
       $$ = FLOATING_POINT_NUMBER_VALUE;
     }
;

// ########### END of the rules for DECLARATIONS OF VARIABLES OF THE MAIN FUNCTION ###########

// ########### START of the rules for DECLARATIONS OF VARIABLES OF OTHER FUNCTIONS ###########

// OLIART
opt_fun_decls : fun_decls
              | /* epsilon */

fun_decls : fun_decls fun_dec
          | fun_dec

fun_dec : RES_WORD_FUN IDENTIFIER SYMBOL_LT_PARENTHESES oparams SYMBOL_RT_PARENTHESES SYMBOL_COLON tipo SYMBOL_LT_BRACKET opt_decls_for_function SYMBOL_RT_BRACKET stmt
        {
          insertToSymbolTable(&mainFunctionSymbolTableHead, (char*)$2, FUNCTION_VALUE, $7, functionSymbolTableHead, $11);
          functionSymbolTableHead = NULL;
        }

oparams : params
        | /* epsilon */

params : param SYMBOL_COMMA params
       | param

param : RES_WORD_VAR IDENTIFIER SYMBOL_COLON tipo
      {
        insertToSymbolTable(&functionSymbolTableHead, (char*)$2, $4, NOTHING, NULL, NULL);
      }

// OLIART

// COPY OF OTHER RULES, BUT FOR THE CASE OF THE SYMBOL TABLES OF FUNCTIONS

opt_decls_for_function : decls_for_function
                        | /* epsilon */
;

decls_for_function : dec_for_function SYMBOL_SEMICOLON decls_for_function 
                    | dec_for_function
;

dec_for_function : RES_WORD_VAR IDENTIFIER SYMBOL_COLON tipo
                {
                  // printf("id name = %s\n", (char*)$2);
                  insertToSymbolTable(&functionSymbolTableHead, (char*)$2, $4, NOTHING, NULL, NULL);
                }
;

// ########### END of the rules for DECLARATIONS OF OTHER FUNCTIONS ###########

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
            | RES_WORD_RETURN expr SYMBOL_SEMICOLON
            {
              $$ = createNode(NOTHING, NOTHING, NULL, RETURN, STMT, $2, NULL, NULL, NULL, NULL);
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
          {
            struct SyntaxTreeNode* idNode = createNode(NOTHING, NOTHING, (char *)$3, ID_VALUE, FOR, NULL, NULL, NULL, NULL, NULL);
            struct SyntaxTreeNode* setNode = createNode(NOTHING, NOTHING, NULL, SET, FOR, idNode, $4, NULL, NULL, NULL);
            struct SyntaxTreeNode* ltNode = createNode(NOTHING, NOTHING, NULL, LEQ, EXPRESION, idNode, $6, NULL, NULL, NULL);
            struct SyntaxTreeNode* stepNode = createNode(NOTHING, NOTHING, NULL, PLUS, EXPR, idNode, $8, NULL, NULL, NULL);
            struct SyntaxTreeNode* setNode2 = createNode(NOTHING, NOTHING, NULL, SET, FOR, idNode, stepNode, NULL, NULL, NULL);
            $$ = createNode(NOTHING, NOTHING, NULL, FOR, ITER_STMT, setNode, ltNode, setNode2, $10, NULL);
          }
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
        $$ = createNode(NOTHING, NOTHING, NULL, STAR, TERM, $1, $3, NULL, NULL, NULL);
     }
     | term SYMBOL_FORWARD_SLASH factor
     {
        $$ = createNode(NOTHING, NOTHING, NULL, FORWARD_SLASH, TERM, $1, $3, NULL, NULL, NULL);
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
          //$1 stores integer number as a pointer we should cast it before creating node.
          $$ = createNode((int)$1, NOTHING, NULL, INTEGER_NUMBER_VALUE, TERM, NULL, NULL, NULL, NULL, NULL);
       }
       | FLOATING_POINT_NUMBER
       {
          $$ = createNode(NOTHING, doubleVal, NULL, FLOATING_POINT_NUMBER_VALUE, TERM, NULL, NULL, NULL, NULL, NULL);
       }
       | IDENTIFIER SYMBOL_LT_PARENTHESES opt_exprs SYMBOL_RT_PARENTHESES
       {
          $$ = createNode(NOTHING, NOTHING, (char *)$1, FUNCTION_VALUE, TERM, $3, NULL, NULL, NULL, NULL);
       }
;

opt_exprs : expr_lst
          {
            $$ = $1;
          }
          | /*epsilon*/
          {
            $$ = NULL;
          }
;

expr_lst : expr_lst SYMBOL_COMMA expr
          {
            $$ = createNode(NOTHING, NOTHING, NULL, PARAMETER_VALUE, EXPR_LST, $3, $1, NULL, NULL, NULL);
          }
          | expr
          {
            $$ = createNode(NOTHING, NOTHING, NULL, PARAMETER_VALUE, EXPR_LST, $1, NULL, NULL, NULL, NULL);
          }
          

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
#define ERROR_CODE_DATA_TYPE_MISMATCH                                       4
#define ERROR_CODE_PARAMETER_SET_MISMATCH                                   5

// Error messages
#define ERROR_MESSAGE_SYMBOL_NOT_FOUND                                      "Attempted to retrieve a non-existent symbol."
#define ERROR_MESSAGE_INVALID_ASSIGNMENT_TO_INT_SYMBOL                      "Attempted to assign an integer value to a symbol storing a floating-point value."
#define ERROR_MESSAGE_INVALID_ASSIGNMENT_TO_FLOATING_POINT_SYMBOL           "Attempted to assign a floating-point value to a symbol storing an integer value."
#define ERROR_MESSAGE_DATA_TYPE_MISMATCH                                    "Attempted to perform an operation with more than one data type."
#define ERROR_MESSAGE_PARAMETER_SET_MISMATCH                                "Attempted to pass an incorrect parameter set to a function"

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
  int returnType;
  union {

    // Functions use the intVal or doubleVal fields
    // for storing the return value (in case it exists).
    int intVal; /* Integer value */
    double doubleVal; /* Floating-point value */
  } value;

  // Pointer to the syntax tree of the function, in case this is a 
  // function node. Whenever the function is called, this separate 
  // syntax tree will be traversed.
  struct SyntaxTreeNode *ptrFunctionSyntaxTreeRootNode; 
  // Pointer to the symbol table of the function, in case this is a
  // function node. This symbol table will contain both the list of 
  // parameters to this function and the list of variables declared
  // inside this function.
  struct SymbolTableNode *ptrFunctionSymbolTableNode; 
  struct SymbolTableNode *next;
};

/**
 * Struct that corresponds to the current function being executed.
 * This represents the call stack.
 */ 
struct CurrentFunction{

  struct SymbolTableNode* ptrFunctionSymbolNode;
  struct CurrentFunction* stack;
  // Flag that indicates whether the function
  // already executed a 'return' statement.
  int alreadyReturned;
  // The return value of the current function MUST be stored
  // in the call stack because if a function is called inside
  // itself or a function is a parameter to itself, its return
  // values could be lost/overwritten.
  union {
    int intVal; /* Integer value */
    double doubleVal; /* Floating-point value */
  } value;
};

/**
 * Function that determines whether a function
 * is being executed.
 * 
 * @returns a positive number if a function is being
 * executed. Else, zero.
 */ 
int functionIsBeingExecuted(){

  return ptrFunctionCallStackTop != NULL;
}

/**
 * Function that determines whether the current function
 * being executed already executed a 'return' statement.
 * 
 * @returns a positive number if the current function
 * being executed already executed a 'return' statement.
 * Else, zero.
 */ 
int currentFunctionAlreadyReturned(){

  return ptrFunctionCallStackTop->alreadyReturned;
}

/**
 * Function that inserts a new symbol to the symbol table.
 * 
 * @param symbolName the name of the symbol to insert.
 * @param symbolType the type of the symbol to insert: int, float or function pointer
 * @returns a pointer to the new node, which will now be the head of the linked list that 
 * represents the symbol table.
 */ 
void insertToSymbolTable(struct SymbolTableNode** ptrPtrTableHead, char const *symbolName, int symbolType, int symbolReturnType, struct SymbolTableNode *ptrFunctionSymbolTableNode, struct SyntaxTreeNode *ptrFunctionSyntaxTreeRootNode){

  // Malloc for the new node
  struct SymbolTableNode* newNodePtr = (struct SymbolTableNode*) malloc(sizeof(struct SymbolTableNode));
  
  // Malloc for the symbol's name
  newNodePtr->name = (char *) malloc(strlen(symbolName) + 1);
  strcpy (newNodePtr->name, symbolName);
  newNodePtr->type = symbolType;

  newNodePtr->returnType = symbolReturnType;

  // Set the default values to 0
  newNodePtr->value.intVal = 0;

  newNodePtr->ptrFunctionSymbolTableNode = ptrFunctionSymbolTableNode;
  newNodePtr->ptrFunctionSyntaxTreeRootNode = ptrFunctionSyntaxTreeRootNode;

  // Insert at the beginning of the list
  newNodePtr->next = (struct SymbolTableNode*)(*ptrPtrTableHead);
  *ptrPtrTableHead = newNodePtr;
}

/**
 * Function that retrieves a symbol from the symbol table given its name.
 * 
 * @param symbolName the name of the symbol to retrieve.
 * @param symbolTableHead the head of the symbol table. This could be either
 * the symbol table of the main function or the symbol table of a function.
 * @returns a pointer to the symbol or 0 if the symbol was not found.
 */ 
struct SymbolTableNode* auxRetrieveFromSymbolTable(char const *symbolName, struct SymbolTableNode* symbolTableHead){

  struct SymbolTableNode *currPtr = symbolTableHead;

  // Traverse the linked list and compare the name of the current symbol
  // with the name of the symbol that you're looking for.
  while(currPtr != NULL){

    assert(currPtr->name);

    if(strcmp(currPtr->name, symbolName) == 0)
      return currPtr;

    currPtr = currPtr->next;
  }

  // If the symbol was not found in the function's symbol table, it must be looked for
  // in the symbol table of the main function.
  // handleError(ERROR_CODE_SYMBOL_NOT_FOUND, ERROR_MESSAGE_SYMBOL_NOT_FOUND);
  return NULL;
}

/**
 * Function that retrieves a symbol from a symbol table given its name.
 * 
 * @param symbolName the name of the symbol to retrieve.
 */ 
struct SymbolTableNode* retrieveFromSymbolTable(char const *symbolName){

  struct SymbolTableNode *currPtr = NULL;

  // If a function is being executed, first look for the symbol in the function's
  // symbol table
  if(functionIsBeingExecuted())
    currPtr = auxRetrieveFromSymbolTable(symbolName, ptrFunctionCallStackTop->ptrFunctionSymbolNode->ptrFunctionSymbolTableNode);

  // If a function is being executed but the symbol was not found in the function's
  // symbol table or if no function is being executed, then look for the symbol in
  // the main function's symbol table.
  if((functionIsBeingExecuted() && !currPtr) || !functionIsBeingExecuted())
    currPtr = auxRetrieveFromSymbolTable(symbolName, mainFunctionSymbolTableHead);

  // If even after searching twice the symbol wasn't found, then error out and exit
  if(!currPtr)
    handleError(ERROR_CODE_SYMBOL_NOT_FOUND, ERROR_MESSAGE_SYMBOL_NOT_FOUND);

  return currPtr;
}

/**
 * Function to print a node of the symbol table.
 * 
 * @param node a pointer to the node to print
 */ 
void printSymbolTableNode(struct SymbolTableNode *node){

  if(node == NULL)
    return;

  printf("Address: %p\n", node);
  printf("Symbol: %s\n", node->name);

  if(node->type < sizeof(SyntaxTreeNodeTypeName)){

    printf("Type: %s\n", SyntaxTreeNodeTypeName[node->type]);
  }
  else{

    printf("Type: %d\n", node->type);
  }

  switch(node->type){

    case INTEGER_NUMBER_VALUE:

      printf("Value: %d\n", node->value.intVal);
      break;

    case FLOATING_POINT_NUMBER_VALUE:

      printf("Value: %lf\n", node->value.doubleVal);
      break;

    case FUNCTION_VALUE:

      assert(node->returnType < sizeof(SyntaxTreeNodeTypeName));
      printf("Return type: %s\n\n", SyntaxTreeNodeTypeName[node->returnType]);
      printSymbolTable(node->ptrFunctionSymbolTableNode, node->name);
      startTreePrinting(node->ptrFunctionSyntaxTreeRootNode, node->name);

      break;
  }

  printf("Next symbol pointer = %p\n", node->next);
  printf("\n");
}

/**
 * Function to print the symbol table
 */ 
void printSymbolTable(struct SymbolTableNode* ptrTableHead, char* tableName){

  printf("########## START OF SYMBOL TABLE OF THE %s FUNCTION ##########\n\n", tableName);
  struct SymbolTableNode *currPtr = ptrTableHead;

  while(currPtr != NULL){

    printSymbolTableNode(currPtr);
    currPtr = currPtr->next;
  }

  printf("########## END OF SYMBOL TABLE OF THE %s FUNCTION ##########\n\n", tableName);
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
    if(symbolPtr->type == INTEGER_NUMBER_VALUE){

      symbolPtr->value.intVal = newIntegerValue;
    }
    else{

      // Error out and exit!
      handleError(ERROR_CODE_INVALID_ASSIGNMENT_TO_INT_SYMBOL, 
        ERROR_MESSAGE_INVALID_ASSIGNMENT_TO_INT_SYMBOL);
    }
  }
  else{

    // Error out and exit!
    handleError(ERROR_CODE_SYMBOL_NOT_FOUND, 
      ERROR_MESSAGE_SYMBOL_NOT_FOUND);
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
    if(symbolPtr->type == FLOATING_POINT_NUMBER_VALUE){

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
 * Function that inserts a function to the call stack
 * @param ptrFunctionSymbolNode a pointer to the symbol corresponding to the function
 * to insert in the call stack
 */ 
void insertFunctionCallToStack(struct SymbolTableNode* ptrFunctionSymbolNode){

  struct CurrentFunction *ptrNewFunctionCallNode = (struct CurrentFunction*) malloc(sizeof(struct CurrentFunction));
  ptrNewFunctionCallNode->ptrFunctionSymbolNode = ptrFunctionSymbolNode;
  ptrNewFunctionCallNode->stack = ptrFunctionCallStackTop;
  ptrNewFunctionCallNode->alreadyReturned = 0;
  ptrNewFunctionCallNode->value.intVal = 0;
  ptrFunctionCallStackTop = ptrNewFunctionCallNode;
}

/**
 * Function that pops a function from the call stack.
 */ 
void popFunctionCallToStack(){

  struct CurrentFunction* tmp = ptrFunctionCallStackTop;
  ptrFunctionCallStackTop = ptrFunctionCallStackTop->stack;
  free(tmp);
}

/**
 * Function that prints the call stack.
 */ 
void printCallStack(){

  printf("Current call stack:\n\n");

  struct CurrentFunction* tmp = ptrFunctionCallStackTop;
  while(tmp != NULL){

    printf("%s\n", tmp->ptrFunctionSymbolNode->name);
    tmp = tmp->stack;
  }

  printf("\n\n");
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
    else if(type == FUNCTION_VALUE){

      // Malloc for the function's name
      newNodePtr->value.idName = (char *) malloc(strlen(idName) + 1);
      strcpy (newNodePtr->value.idName, idName);
    }

    return newNodePtr;
}

/**
 * Function that prints the node type of a given node.
 * 
 * @param type the type of node
 * @param label a label to display with the type
 */ 
void printNodeType(int type, char* label){

  // If our names array contains an entry for this type
  if(type >= 0 && type < sizeof(SyntaxTreeNodeTypeName)){

    printf("%s: %s\n", label, SyntaxTreeNodeTypeName[type]);
  }
  else{

    printf("%s: %d\n", label, type);
  }
}

/**
 * Function that starts the printing a given node of a syntax tree root node
 * 
 * @param syntaxTreeRootNode the root node of the tree to be printed.
 */ 
void startTreePrinting(struct SyntaxTreeNode* syntaxTreeRootNode, char* functionName){

  printf("########## START OF SYNTAX TREE OF THE %s FUNCTION ##########\n\n", functionName);
  printTree(syntaxTreeRootNode);
  printf("########## END OF SYNTAX TREE OF THE %s FUNCTION ##########\n\n", functionName);
}

/**
 * Function that prints a given node of the syntax tree.
 * 
 * @param node the node to be printed.
 */ 
void printTree(struct SyntaxTreeNode* node){

  if(node == NULL)
    return;

  printNodeType(node->type, "type");
  printf("address = %p\n", node);
  printNodeType(node->parentNodeType, "parentNodeType");

  if(node->type == INTEGER_NUMBER_VALUE){

    printf("Node value = %d\n", node->value.intVal);
  }

  else if(node->type == ID_VALUE){

    printf("Node value = %s\n", node->value.idName);
  }

  else if(node->type == FLOATING_POINT_NUMBER_VALUE){

    printf("Node value = %f\n", node->value.doubleVal);
  }
  
  else if(node->type == FUNCTION_VALUE){

    printf("Node value = %s\n", node->value.idName);
  }

  // Print the addresses of the current node's children
  int i = 0;
  for(i = 0; i < 4; i++)
    printf("ptr #%d: %p\n", i + 1, node->arrPtr[i]);

  printf("\n");

  // Now call the printing of the current node's children
  for(i = 0; i < 4; i++)
    printTree(node->arrPtr[i]);
}

/**
 * Function that computes a given expr term of integer
 * numbers.
 * 
 * @param exprDoubleNode the root node of the 'expr' term of integers.
 * @returns the integer value that results when the expression
 * is evaluated.
 */ 
int func_exprInt(struct SyntaxTreeNode* exprIntNode){

  // If we enter an EXPR node, we must at least one term.
  assert(exprIntNode != NULL);

  if(exprIntNode->type == PLUS){

    return func_exprInt(exprIntNode->arrPtr[0]) 
      + func_exprInt(exprIntNode->arrPtr[1]);
  }
  else if(exprIntNode->type == MINUS){

    return func_exprInt(exprIntNode->arrPtr[0]) 
      - func_exprInt(exprIntNode->arrPtr[1]);
  }
  else if(exprIntNode->type == STAR){

    return func_exprInt(exprIntNode->arrPtr[0]) 
      * func_exprInt(exprIntNode->arrPtr[1]);
  }
  else if(exprIntNode->type == FORWARD_SLASH){

    return func_exprInt(exprIntNode->arrPtr[0]) 
      / func_exprInt(exprIntNode->arrPtr[1]);
  }

  assert(exprIntNode->type == INTEGER_NUMBER_VALUE
    || exprIntNode->type == ID_VALUE
    || exprIntNode->type == FUNCTION_VALUE);

  int valToReturn = 0;

  if(exprIntNode->type == INTEGER_NUMBER_VALUE){

    valToReturn = exprIntNode->value.intVal;
  }
  else if(exprIntNode->type == ID_VALUE){

    struct SymbolTableNode *currNode = retrieveFromSymbolTable(exprIntNode->value.idName);
    assert(currNode->type == INTEGER_NUMBER_VALUE);
    //printSymbolTableNode(currNode);
    valToReturn = currNode->value.intVal;
  }
  else if(exprIntNode->type == FUNCTION_VALUE){

    func_func(exprIntNode);
    // As we know that this is a function, then we know that this symbol must exist in the symbol table
    // of the main function, so directly call the auxiliary method that looks in that specific symbol table.
    struct SymbolTableNode* currFunc = auxRetrieveFromSymbolTable(exprIntNode->value.idName, mainFunctionSymbolTableHead);
    assert(currFunc->returnType == INTEGER_NUMBER_VALUE);    
    // valToReturn = currFunc->value.intVal;
    valToReturn = ptrFunctionCallStackTop->value.intVal;
    popFunctionCallToStack();
  }

  return valToReturn;
}

/**
 * Function that computes a given expr term of floating-point
 * numbers.
 * 
 * @param exprDoubleNode the root node of the 'expr' term of doubles.
 * @returns the floating-point value that results when the expression
 * is evaluated.
 */ 
double func_exprDouble(struct SyntaxTreeNode* exprDoubleNode){
  // If we enter an EXPR node, we must at least one term.
  assert(exprDoubleNode != NULL);

  if(exprDoubleNode->type == PLUS){

    return func_exprDouble(exprDoubleNode->arrPtr[0]) 
      + func_exprDouble(exprDoubleNode->arrPtr[1]);
  }
  else if(exprDoubleNode->type == MINUS){

    return func_exprDouble(exprDoubleNode->arrPtr[0]) 
      - func_exprDouble(exprDoubleNode->arrPtr[1]);
  }
  else if(exprDoubleNode->type == STAR){

    return func_exprDouble(exprDoubleNode->arrPtr[0]) 
      * func_exprDouble(exprDoubleNode->arrPtr[1]);
  }
  else if(exprDoubleNode->type == FORWARD_SLASH){

    return func_exprDouble(exprDoubleNode->arrPtr[0]) 
      / func_exprDouble(exprDoubleNode->arrPtr[1]);
  }

  assert(exprDoubleNode->type == ID_VALUE
    || exprDoubleNode-> type == FLOATING_POINT_NUMBER_VALUE
    || exprDoubleNode-> type == FUNCTION_VALUE);

  // handleError(ERROR_CODE_DATA_TYPE_MISMATCH, ERROR_MESSAGE_DATA_TYPE_MISMATCH);

  double valToReturn = 0;

  if(exprDoubleNode->type == FLOATING_POINT_NUMBER_VALUE){
    valToReturn = exprDoubleNode->value.doubleVal;
  }
  else if(exprDoubleNode->type == ID_VALUE){

    struct SymbolTableNode *currNode = retrieveFromSymbolTable(exprDoubleNode->value.idName);
    assert(currNode->type == FLOATING_POINT_NUMBER_VALUE);
    //printSymbolTableNode(currNode);
    valToReturn = currNode->value.doubleVal;
  }
  else if(exprDoubleNode-> type == FUNCTION_VALUE){

    func_func(exprDoubleNode);
    // As we know that this is a function, then we know that this symbol must exist in the symbol table
    // of the main function, so directly call the auxiliary method that looks in that specific symbol table.
    struct SymbolTableNode* currFunc = auxRetrieveFromSymbolTable(exprDoubleNode->value.idName, mainFunctionSymbolTableHead);
    assert(currFunc->returnType == FLOATING_POINT_NUMBER_VALUE);    
    // valToReturn = currFunc->value.doubleVal;
    valToReturn = ptrFunctionCallStackTop->value.doubleVal;
    popFunctionCallToStack();
  }

  return valToReturn;
}

/**
 * Function that computes the amount of nodes of a given type in a syntax sub tree.
 * 
 * @param nodeType the type of node to count.
 * @param node a pointer to the root node of the corresponding subtree.
 * @returns count the amount of nodes of a given type in a syntax sub tree.
 */ 
int computeSubTreeNodeTypeCount(int nodeType, struct SyntaxTreeNode* node){

  if(node == NULL)
    return 0;

  int count = 0;
  
  // In case it is a constant
  if(node->type == nodeType){

    count++;
  }
  
  // In case it is an identifier
  else if(node->type == ID_VALUE){

    struct SymbolTableNode* currIdNode = retrieveFromSymbolTable(node->value.idName);
    
    if(currIdNode->type == nodeType)
      count++;
  }

  int i = 0;
  for(i = 0; i < 4; i++){

    count += computeSubTreeNodeTypeCount(nodeType, node->arrPtr[i]);
  }

  return count;
}

int exprIsTypeConsistent(struct SyntaxTreeNode* exprNode){

  // Count the number of subtree nodes of both data type
  // If the expression is type-consistent, then one of those counts
  // will be zero
  int intSubTreeNodeCount = computeSubTreeNodeTypeCount(INTEGER_NUMBER_VALUE, exprNode);
  int doubleSubTreeNodeCount = computeSubTreeNodeTypeCount(FLOATING_POINT_NUMBER_VALUE, exprNode);

  // printTree(exprNode);
  // printf("intSubTreeNodeCount: %d; doubleSubTreeNodeCount: %d\n", intSubTreeNodeCount, doubleSubTreeNodeCount);

  if(intSubTreeNodeCount > 0 && doubleSubTreeNodeCount == 0)
    return INTEGER_NUMBER_VALUE;

  else if(intSubTreeNodeCount == 0 && doubleSubTreeNodeCount > 0)
    return FLOATING_POINT_NUMBER_VALUE;

  // If the control gets here, there is a mistake: types are inconsistent
  // Error out and exit!
  handleError(ERROR_CODE_DATA_TYPE_MISMATCH, ERROR_MESSAGE_DATA_TYPE_MISMATCH);

  return 0;
}

/**
 * Function that determines whether all the terms of a given
 * 'expr' are integer numbers.
 * 
 * @param exprNode the root node of the expr
 * @returns 1 if all terms are integers. Else, 0.
 */ 
int isIntegerExpr(struct SyntaxTreeNode* exprNode){

  return exprIsTypeConsistent(exprNode) == INTEGER_NUMBER_VALUE;
}

/**
 * Function that determines whether all the terms of a given
 * 'expr' are floating-point numbers.
 * 
 * @param exprNode the root node of the expr
 * @returns 1 if all terms are floating-point numbers. Else, 0.
 */ 
int isFloatingPointExpr(struct SyntaxTreeNode* exprNode){

  return exprIsTypeConsistent(exprNode) == FLOATING_POINT_NUMBER_VALUE;
}

/**
 * Function that computes the length of the symbol table of a given 
 * function. 
 * 
 * @param functionName the name of the function whose symbol table's size
 * is to be computed.
 * @returns symbolTableLength the length of the symbol table of that function.
 */ 
int computeFunctionSymbolTableLength(char const *functionName){

  // As we know that this is a function, then we know that this symbol must exist in the symbol table
  // of the main function, so directly call the auxiliary method that looks in that specific symbol table.
  struct SymbolTableNode* functionSymbol = auxRetrieveFromSymbolTable(functionName, mainFunctionSymbolTableHead);
  assert(functionSymbol);

  struct SymbolTableNode* currSymbolInFunction = functionSymbol->ptrFunctionSymbolTableNode;

  int symbolTableLength = 0;

  while(currSymbolInFunction != NULL){

    symbolTableLength++;
    currSymbolInFunction = currSymbolInFunction->next;
  }

  return symbolTableLength;
}

/**
 * Function that computes the amount of parameters passed to a function
 * 
 * @param node a node in the syntax subtree corresponding to 
 * the called function
 */ 
int computeAmountOfParametersPassed(struct SyntaxTreeNode* node, int depthFromFunctionRootNode){

  if(!node)
    return 0;
  
  // Careful: what if a parameter to a function is another function?
  // There's a bug here!!

  int count = 0;

  if(depthFromFunctionRootNode <= 1)
    count += (node->type == PARAMETER_VALUE);

  for(int i = 0; i < 4; i++)
    count += computeAmountOfParametersPassed(node->arrPtr[i], depthFromFunctionRootNode + (node->type == FUNCTION_VALUE));

  return count;
}

/**
 * Function that moves a pointer to a symbol in a function's symbol table a specified
 * amount of symbols to the right.
 * 
 * @param ptrPtrFunctionSymbolTableSymbol a pointer to the pointer of the first symbol 
 * in the function's symbol table.
 * @param amountOfMoves the required amount of moves to the right
 */ 
void moveSymbolTableNodePointerForward(struct SymbolTableNode** ptrPtrFunctionSymbolTableSymbol, int amountOfMoves){

  for(int i = 0; i < amountOfMoves; i++)
    *ptrPtrFunctionSymbolTableSymbol = (*ptrPtrFunctionSymbolTableSymbol)->next;
}

/**
 * Function that retrieves the next passed parameter in a function call.
 * 
 * @param ptrPtrFuncNodeSubStree a pointer to a pointer to the sub tree of a function call.
 * The idea of the double pointer is to keep track of which passed parameters have already 
 * been analyzed.
 * @returns a pointer to the next passed parameter
 */ 
struct SyntaxTreeNode* getNextPassedParameter(struct SyntaxTreeNode** ptrPtrFuncNodeSubStree){

  assert(*ptrPtrFuncNodeSubStree);
  struct SyntaxTreeNode *ptrNextPassedParameter = NULL;

  if((*ptrPtrFuncNodeSubStree)->type == FUNCTION_VALUE){

    assert((*ptrPtrFuncNodeSubStree)->arrPtr[0]);
    ptrNextPassedParameter = (*ptrPtrFuncNodeSubStree)->arrPtr[0]->arrPtr[0];
  }
  else{

    assert((*ptrPtrFuncNodeSubStree)->type == PARAMETER_VALUE);
    ptrNextPassedParameter = (*ptrPtrFuncNodeSubStree)->arrPtr[0];
  }

  (*ptrPtrFuncNodeSubStree) = (*ptrPtrFuncNodeSubStree)->arrPtr[0]->arrPtr[1];

  assert(ptrNextPassedParameter);
  return ptrNextPassedParameter;
}

/**
 * Function that determines whether the set of parameters passed to 
 * a function is correct.
 * 
 * @param funcNode the root node of the called function
 * @returns 1 if the parameter set is correct. Else, 0.
 */ 
int parametersAreCorrect(struct SyntaxTreeNode* funcNode){

  // 1. Get S, the size of the linked list of the function's symbol table.
  // 2. Get A, the amount of parameters passed to the called function.
  // 
  // Note: S should always be greater than or equal to A. S is greater than A
  // when the function has variable declarations.
  // 
  // 3. Now, the amount of formal parameters in the function must be exactly
  //    equal to A. Besides, both the formal parameters of the function 
  //    and the actual parameter values passed to the function are stored 
  //    in reverse order. So, after moving forward (S - A) nodes in the 
  //    function's symbol table, a simple check between the current formal 
  //    parameter in the function's symbol table and the actual parameter 
  //    in the syntax tree of the main function should suffice to determine
  //    whether the set of parameter values that were passed is correct.

  // 'functionSymbolTableLength' corresponds to 'S' in the above explanation
  int functionSymbolTableLength = computeFunctionSymbolTableLength(funcNode->value.idName);
  printf("functionSymbolTableLength = %d\n", functionSymbolTableLength);
  // 'amountOfParametersPassed' corresponds to 'A' in the above explanation
  int amountOfParametersPassed = computeAmountOfParametersPassed(funcNode, 0);
  printf("amountOfParametersPassed = %d\n", amountOfParametersPassed);
  assert(functionSymbolTableLength >= amountOfParametersPassed);

  // As we know that this is a function, then we know that this symbol must exist in the symbol table
  // of the main function, so directly call the auxiliary method that looks in that specific symbol table.
  struct SymbolTableNode* ptrCurrFormalParameter = auxRetrieveFromSymbolTable(funcNode->value.idName, mainFunctionSymbolTableHead)->ptrFunctionSymbolTableNode;
  moveSymbolTableNodePointerForward(&ptrCurrFormalParameter, functionSymbolTableLength - amountOfParametersPassed);

  struct SyntaxTreeNode* ptrFuncNodeSubStree = funcNode;
  struct SyntaxTreeNode* ptrCurrPassedParameter = NULL;

  for(int i = 0; i < amountOfParametersPassed; i++){

    ptrCurrPassedParameter = getNextPassedParameter(&ptrFuncNodeSubStree);
    assert(ptrCurrFormalParameter);
    assert(ptrCurrPassedParameter);

    // Check whether the value of the 'expr' term passed as a parameter matches the expected 
    // formal parameter data type.
    if((isIntegerExpr(ptrCurrPassedParameter) && ptrCurrFormalParameter->type != INTEGER_NUMBER_VALUE)
    || (isFloatingPointExpr(ptrCurrPassedParameter) && ptrCurrFormalParameter->type != FLOATING_POINT_NUMBER_VALUE)){

      return 0;
    }

    ptrCurrFormalParameter = ptrCurrFormalParameter->next;
  }

  return 1;
}

/**
 * Function that assigns parameter to a function call.
 * 
 * Note: For each function call, the function parameters 
 * are stored as child nodes in the syntax tree of the 
 * caller (function). To pass the parameters to the function,
 * the parameter values are assigned to the symbol table 
 * of the callee (function).
 */ 
void assignParameters(struct SyntaxTreeNode* funcNode){

  // 1. Get the size of the linked list of the function's symbol table.
  int functionSymbolTableLength = computeFunctionSymbolTableLength(funcNode->value.idName);

  // 2. Get the amount of parameters passed to the called function.
  int amountOfParametersPassed = computeAmountOfParametersPassed(funcNode, 0);

  // functionSymbolTableLength should always be greater than or equal to amountOfParametersPassed. 
  // functionSymbolTableLength is greater than amountOfParametersPassed when the function has variable
  // declarations.
  assert(functionSymbolTableLength >= amountOfParametersPassed);

  // Get a pointer to the first formal parameter to which a value will be assigned. Remember, parameters
  // are stored in reverse order.
  struct SymbolTableNode* ptrCurrFormalParameter = auxRetrieveFromSymbolTable(funcNode->value.idName, mainFunctionSymbolTableHead)->ptrFunctionSymbolTableNode;
  moveSymbolTableNodePointerForward(&ptrCurrFormalParameter, functionSymbolTableLength - amountOfParametersPassed);

  struct SyntaxTreeNode* ptrFuncNodeSubStree = funcNode;
  struct SyntaxTreeNode* ptrCurrPassedParameter = NULL;

  for(int i = 0; i < amountOfParametersPassed; i++){

    ptrCurrPassedParameter = getNextPassedParameter(&ptrFuncNodeSubStree);
    assert(ptrCurrFormalParameter);
    assert(ptrCurrPassedParameter);

    // Assign the passed values to the function's symbol table
    if(ptrCurrFormalParameter->type == INTEGER_NUMBER_VALUE){

      // Assign to the formal parameter the integer value of the 'expr' term passed 
      // as a parameter
      ptrCurrFormalParameter->value.intVal = func_exprInt(ptrCurrPassedParameter);
    }
    else{

      assert(ptrCurrFormalParameter->type == FLOATING_POINT_NUMBER_VALUE);
      // Assign to the formal parameter the double value of the 'expr' term passed 
      // as a parameter
      ptrCurrFormalParameter->value.doubleVal = func_exprDouble(ptrCurrPassedParameter);
    }

    ptrCurrFormalParameter = ptrCurrFormalParameter->next;
  }
}

/**
 * Function that handles function calls.
 * 
 * @param funcNode the root node of the function call
 */ 
void func_func(struct SyntaxTreeNode* funcNode){

  // Check whether the passed parameters are correct
  if(!parametersAreCorrect(funcNode))
    handleError(ERROR_CODE_PARAMETER_SET_MISMATCH, ERROR_MESSAGE_PARAMETER_SET_MISMATCH);

  // Assign the parameters
  assignParameters(funcNode);

  // Get the symbol of the function
  // As we know that this is a function, then we know that this symbol must exist in the symbol table
  // of the main function, so directly call the auxiliary method that looks in that specific symbol table.
  struct SymbolTableNode* funcSymbol = auxRetrieveFromSymbolTable(funcNode->value.idName, mainFunctionSymbolTableHead);
  
  // Add the function to the call stack
  insertFunctionCallToStack(funcSymbol);
  
  #ifdef _PRINT_CALL_STACK
  printCallStack();
  #endif
  
  // Execute the function
  traverseTree(funcSymbol->ptrFunctionSyntaxTreeRootNode);
  
  // // Pop the current function from the call stack
  // popFunctionCallToStack();
}

/**
 * Function that handles print statements of the 
 * language's grammar.
 * 
 * @param printNode the root node of the print statement
 */ 
void func_print(struct SyntaxTreeNode* printNode){

  // If we enter a PRINT node, we must have an 'expr' term
  // for printing.
  assert(printNode->arrPtr[0] != NULL);

  if(printNode->arrPtr[0]->type == INTEGER_NUMBER_VALUE){

    printf("%d\n", printNode->arrPtr[0]->value.intVal);
  } 
  else if(printNode->arrPtr[0]->type == FLOATING_POINT_NUMBER_VALUE){
    printf("%f\n", printNode->arrPtr[0]->value.doubleVal);
  }
  else if(printNode->arrPtr[0]->parentNodeType == EXPR
    || printNode->arrPtr[0]->parentNodeType == TERM
    || printNode->arrPtr[0]->parentNodeType == FACTOR){
      
    // If this is in fact a function call, instead of an expression itself
    if(printNode->arrPtr[0]->type == FUNCTION_VALUE){ 

      func_func(printNode->arrPtr[0]);
      // As we know that this is a function, then we know that this symbol must exist in the symbol table
      // of the main function, so directly call the auxiliary method that looks in that specific symbol table.
      struct SymbolTableNode* currFunc = auxRetrieveFromSymbolTable(printNode->arrPtr[0]->value.idName, mainFunctionSymbolTableHead);
      
      if(currFunc->returnType == INTEGER_NUMBER_VALUE){

        // printf("%d\n", currFunc->value.intVal);
        printf("%d\n", ptrFunctionCallStackTop->value.intVal);
      }
      else{

        assert(currFunc->returnType == FLOATING_POINT_NUMBER_VALUE);
        // printf("%lf\n", currFunc->value.doubleVal);
        printf("%lf\n", ptrFunctionCallStackTop->value.doubleVal);
      }

      popFunctionCallToStack();
    }
    else{ // If it is an expression

      if(isIntegerExpr(printNode->arrPtr[0])){

        printf("%d\n", func_exprInt(printNode->arrPtr[0]));
      }
      else{

        assert(isFloatingPointExpr(printNode->arrPtr[0]));
        printf("%lf\n", func_exprDouble(printNode->arrPtr[0]));
      }
    }
  }
}

/**
 * Function that handles 'expresion' terms.
 * 
 * @param expresionNode the root node of the 'expresion' term.
 * @returns A positive number is the expression is true. Else, 0.
 */ 
int func_expresion(struct SyntaxTreeNode* expresionNode){

  // If we enter an EXPRESION node, we must have two 'expr' terms
  assert(expresionNode->arrPtr[0] != NULL);
  assert(expresionNode->arrPtr[1] != NULL);

  if(isIntegerExpr(expresionNode->arrPtr[0])){

    // Assert that the second 'expr' term also contains an integer expression
    assert(isIntegerExpr(expresionNode->arrPtr[1]));
    int intExpresionLeftSide = func_exprInt(expresionNode->arrPtr[0]);
    int intExpresionRightSide = func_exprInt(expresionNode->arrPtr[1]);

    switch(expresionNode->type){

      case LT:
        return intExpresionLeftSide < intExpresionRightSide;

      case GT:
        return intExpresionLeftSide > intExpresionRightSide;

      case EQ:
        return intExpresionLeftSide == intExpresionRightSide;

      case LEQ:
        return intExpresionLeftSide <= intExpresionRightSide;

      case GEQ:
        return intExpresionLeftSide >= intExpresionRightSide;
    }
  }
  else{

    assert(isFloatingPointExpr(expresionNode->arrPtr[0]));

    // Assert that the second 'expr' term also contains an floating-point expression
    assert(isFloatingPointExpr(expresionNode->arrPtr[1]));

    double doubleExpresionLeftSide = func_exprDouble(expresionNode->arrPtr[0]);
    int doubleExpresionRightSide = func_exprDouble(expresionNode->arrPtr[1]);

    switch(expresionNode->type){

      case LT:
        return doubleExpresionLeftSide < doubleExpresionRightSide;

      case GT:
        return doubleExpresionLeftSide > doubleExpresionRightSide;

      case EQ:
        return doubleExpresionLeftSide == doubleExpresionRightSide;

      case LEQ:
        return doubleExpresionLeftSide <= doubleExpresionRightSide;

      case GEQ:
        return doubleExpresionLeftSide >= doubleExpresionRightSide;
    }
  }

  // Control should never reach this part of the function.
  assert(NULL);
  return -1;
}

/**
 * Function that handles 'while' terms.
 * 
 * @param whileNode the root node of the 'while' term.
 */ 
void func_while(struct SyntaxTreeNode* whileNode){

  // If we enter a WHILE node, we must have an 'expresion' 
  assert(whileNode->arrPtr[0] != NULL);

  while(func_expresion(whileNode->arrPtr[0])){

    traverseTree(whileNode->arrPtr[1]);
  }
}

/**
 * Function that handles 'if' terms.
 * 
 * @param ifNode the root node of the 'if' term.
 */ 
void func_if(struct SyntaxTreeNode* ifNode){

  // If we enter an IF node, we must have an 'expresion' term
  assert(ifNode->arrPtr[0] != NULL);

  // It is possible to have if without an'stmt' term
  //assert(ifNode->arrPtr[1] != NULL);

  if(func_expresion(ifNode->arrPtr[0])){

    // If the node has a pointer to stmt, execute that node.
    if(ifNode->arrPtr[1] != NULL)
    	traverseTree(ifNode->arrPtr[1]);
  }
}

/**
 * Function that handles 'ifelse' terms.
 * 
 * @param ifElseNode the root node of the 'ifelse' term.
 */ 
void func_ifElse(struct SyntaxTreeNode* ifElseNode){

  // If we enter an IF node, we must have an 'expresion' term
  assert(ifElseNode->arrPtr[0] != NULL);
  
  // It is possible to have if and else without an 'stmt' term
  //assert(ifElseNode->arrPtr[1] != NULL);
  //assert(ifElseNode->arrPtr[2] != NULL);

  if(func_expresion(ifElseNode->arrPtr[0])){

    traverseTree(ifElseNode->arrPtr[1]);
  }
  else{

    traverseTree(ifElseNode->arrPtr[2]);
  }
}

/**
 * Function that handles 'set' terms.
 * 
 * @param setNode the root node of the 'set' term.
 */ 
void func_set(struct SyntaxTreeNode* setNode){

  // Each set statement must contain both the corresponding
  // id and the desired value
  assert(setNode->arrPtr[0] != NULL);
  assert(setNode->arrPtr[1] != NULL);

  // printf("symbol to retrieve: %s\n", setNode->arrPtr[0]->value.idName);
  struct SymbolTableNode* currNode = retrieveFromSymbolTable(setNode->arrPtr[0]->value.idName);
  
  assert(currNode != NULL);

  int exprValueToSet;
  double exprDoubleValueToSet;

  switch(currNode->type){

    case INTEGER_NUMBER_VALUE:
      exprValueToSet = func_exprInt(setNode->arrPtr[1]);
      setIntValueToSymbol(currNode->name, exprValueToSet);
      //printSymbolTableNode(currNode);
      assert(exprValueToSet == currNode->value.intVal);
      break;

    case FLOATING_POINT_NUMBER_VALUE:
      exprDoubleValueToSet = func_exprDouble(setNode->arrPtr[1]);
      setDoubleValueToSymbol(currNode->name, exprDoubleValueToSet);
      //printSymbolTableNode(currNode);
      assert(exprDoubleValueToSet == currNode->value.doubleVal);
      break;
  }
}

/**
 * Function to read an integer and assert that the 
 * reading was successful.
 * 
 * @returns intVal the integer read
 */ 
int readInteger(){

  int intVal = -1;
  printf("Insert an integer: ");
  int scanfReturnValue = scanf("%d", &intVal);
  assert(scanfReturnValue > 0);
  return intVal;
}

/**
 * Function to read a double and assert that the 
 * reading was successful.
 * 
 * @returns doubleVal the double read
 */ 
double readDouble(){

  double doubleVal = -1.0;
  printf("Insert an floating point number: ");
  int scanfReturnValue = scanf("%lf", &doubleVal);
  assert(scanfReturnValue > 0);
  return doubleVal;
}

/**
 * Function that handles 'read' terms.
 * 
 * @param readNode the root node of the 'read' term.
 */ 
void func_read(struct SyntaxTreeNode* readNode){

  // Each set statement must contain the corresponding
  // id to be read.
  assert(readNode->arrPtr[0] != NULL);

  struct SymbolTableNode* currNode = retrieveFromSymbolTable(readNode->arrPtr[0]->value.idName);

  int valueToSet;
  double doubleValueToSet;

  switch(currNode->type){

    case INTEGER_NUMBER_VALUE:
      valueToSet = readInteger();
      setIntValueToSymbol(currNode->name, valueToSet);
      //printSymbolTableNode(currNode);
      assert(valueToSet == currNode->value.intVal);
      break;

    case FLOATING_POINT_NUMBER_VALUE:
      doubleValueToSet = readDouble();
      setDoubleValueToSymbol(currNode->name, doubleValueToSet);
      //printSymbolTableNode(currNode);
      assert(doubleValueToSet == currNode->value.doubleVal);
      break;
  }
}

/**
 * Function that handles 'for' terms.
 * 
 * @param whileNode the root node of the 'for' term.
 */ 
void func_for(struct SyntaxTreeNode* forNode){

  // If we enter a FOR node, we must have 3 'expr' terms
  assert(forNode->arrPtr[0] != NULL);
  assert(forNode->arrPtr[1] != NULL);
  assert(forNode->arrPtr[2] != NULL);

  func_set(forNode->arrPtr[0]);

  while(func_expresion(forNode->arrPtr[1])){

    traverseTree(forNode->arrPtr[3]);
    func_set(forNode->arrPtr[2]);
  }
}

/**
 * Function that handles 'return' statements. The return value of each 
 * function will be stored in the function's entry in the symbol table.
 * 
 * @param returnNode the root node of the 'return' term.
 */ 
void func_return(struct SyntaxTreeNode* returnNode){

  // Each return statement must contain an expression.
  assert(returnNode->arrPtr[0] != NULL);
  struct CurrentFunction* currFunction = ptrFunctionCallStackTop;

  if(returnNode->arrPtr[0]->type == INTEGER_NUMBER_VALUE){

    assert(currFunction->ptrFunctionSymbolNode->returnType == INTEGER_NUMBER_VALUE);
    currFunction->ptrFunctionSymbolNode->value.intVal = returnNode->arrPtr[0]->value.intVal;
  } 
  else if(returnNode->arrPtr[0]->type == FLOATING_POINT_NUMBER_VALUE){
    
    assert(currFunction->ptrFunctionSymbolNode->returnType == FLOATING_POINT_NUMBER_VALUE);
    currFunction->ptrFunctionSymbolNode->value.doubleVal = returnNode->arrPtr[0]->value.doubleVal;
  }
  else if(returnNode->arrPtr[0]->parentNodeType == EXPR
    || returnNode->arrPtr[0]->parentNodeType == TERM
    || returnNode->arrPtr[0]->parentNodeType == FACTOR){
      
    // If this is in fact a function call, instead of an expression itself
    if(returnNode->arrPtr[0]->type == FUNCTION_VALUE){ 

      // TO DO
    }
    else{ // If it is an expression

      if(isIntegerExpr(returnNode->arrPtr[0])){

        assert(currFunction->ptrFunctionSymbolNode->returnType == INTEGER_NUMBER_VALUE);
        currFunction->ptrFunctionSymbolNode->value.intVal = func_exprInt(returnNode->arrPtr[0]);
      }
      else{

        assert(isFloatingPointExpr(returnNode->arrPtr[0]));
        assert(currFunction->ptrFunctionSymbolNode->returnType == FLOATING_POINT_NUMBER_VALUE);
        currFunction->ptrFunctionSymbolNode->value.doubleVal = func_exprDouble(returnNode->arrPtr[0]);
      }
    }
  }

  // Mark that this function already executed a 'return' statement
  // to skip the rest of the function body.
  ptrFunctionCallStackTop->alreadyReturned = 1;
}

/**
 * Function that traverses the syntax tree and
 * actually calls the execution of the input program
 * 
 * @param node the current node to which the execution control 
 * is passed.
 */ 
void traverseTree(struct SyntaxTreeNode* node){

  if(node == NULL)
    return;

  // Check the following:
  // 
  // 1. whether there is a function being executed at the moment 
  // 2. whether the execution control in the current function 
  //   already entered a 'return' statement
  //
  // These checks have the following goals:
  //
  // 1. Making each function return the value of the expression
  //    right after the FIRST 'return' statement.
  //
  // 2. Skipping the execution of the rest of the function if a
  //    'return' statement has already been executed.
  if(functionIsBeingExecuted() && currentFunctionAlreadyReturned())
    return;

  switch(node->type){

    case PROGRAM:

      break;

    case PRINT:

      func_print(node);
      break;

    case IF:

      func_if(node);
      break;

    case IFELSE:

      func_ifElse(node);
      break;

    case WHILE:

      func_while(node);
      break;

    case FOR:

      func_for(node);
      break;

    case SET:

      func_set(node);
      break;

    case READ:

      func_read(node);
      break;

    case RETURN:

      func_return(node);
      break;
  } 

  // Control nodes will call their children in their 
  // respective functions. Skip these explicit calls
  // here.
  if(node->type != IF
    && node->type != IFELSE
    && node->type != WHILE
    && node->type != FOR){

    // Call the traversal of the current node's children.
    int i;
    for(i = 0; i < 4; i++)
      traverseTree(node->arrPtr[i]);
  }
}

int yyerror(char const * s) {
  fprintf(stderr, "Error: %s\n", s);
  return 0;
}

/**
 * Function for handling input either from arguments to the main() function
 * or from standard input (e.g. redirection to file.)
 * 
 * @param argc the amount of arguments that the main() function received.
 * @param argv the pointer to pointer to char corresponding to the arguments 
 * that the main() function received.
 */ 
void handleInput(int argc, char **argv){

    // If an input file was passed
    if(argc > 1){

      // Open the input file for Lex
      yyin = fopen(argv[1], "r");
    }
    else{
        
      // Else, just use standard input
      yyin = stdin;
    }
}

/**
 * Main function of the program.
 */ 
int main(int argc, char **argv) {

  #ifdef _PRINT_PARSE_TRACE
  extern int yydebug;
  yydebug = 1;
  #endif
  handleInput(argc, argv);
  yyparse();
  return 0;
}
