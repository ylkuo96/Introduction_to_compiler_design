%{
/**
 * Introduction to Compiler Design by Prof. Yi Ping You
 * Project 3 YACC sample
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "header.h"
#include "symtab.h"
#include "semcheck.h"

int yydebug;

extern int linenum;     /* declared in lex.l */
extern FILE *yyin;      /* declared by lex */
extern char *yytext;    /* declared by lex */
extern char buf[256];   /* declared in lex.l */
extern int yylex(void);
int yyerror(char*);

int scope = 0;
int Opt_D = 1;                  // symbol table dump option
char fileName[256];             // filename of input file
struct SymTable *symbolTable;	// main symbol table
__BOOLEAN paramError;			// indicate is parameter have any error?
struct PType *funcReturn;		// record return type of function, used at 'return statement' production rule

// ---
extern FILE* jasmin;
int localStack = 1; // local variable number
int layer[100]; // for labels count
int top = -1; // for current layer
int labelCount= -1;
__BOOLEAN R=__FALSE; // for READ can not put things on stack
%}

%union {
	int intVal;
	float realVal;
	//__BOOLEAN booleanVal;
	char *lexeme;
	struct idNode_sem *id;
	//SEMTYPE type;
	struct ConstAttr *constVal;
	struct PType *ptype;
	struct param_sem *par;
	struct expr_sem *exprs;
	/*struct var_ref_sem *varRef; */
	struct expr_sem_node *exprNode;
};

/* tokens */
%token ARRAY BEG BOOLEAN DEF DO ELSE END FALSE FOR INTEGER IF OF PRINT READ REAL RETURN STRING THEN TO TRUE VAR WHILE
%token OP_ADD OP_SUB OP_MUL OP_DIV OP_MOD OP_ASSIGN OP_EQ OP_NE OP_GT OP_LT OP_GE OP_LE OP_AND OP_OR OP_NOT
%token MK_COMMA MK_COLON MK_SEMICOLON MK_LPAREN MK_RPAREN MK_LB MK_RB

%token <lexeme>ID
%token <intVal>INT_CONST 
%token <realVal>FLOAT_CONST
%token <realVal>SCIENTIFIC
%token <lexeme>STR_CONST

%type<id> id_list
%type<constVal> literal_const
%type<ptype> type scalar_type array_type opt_type
%type<par> param param_list opt_param_list
%type<exprs> var_ref boolean_expr boolean_term boolean_factor relop_expr expr term factor boolean_expr_list opt_boolean_expr_list
%type<intVal> dim mul_op add_op rel_op array_index loop_param

/* start symbol */
%start program
%%

program			: ID
			{
			  struct PType *pType = createPType (VOID_t);
			  struct SymNode *newNode = createProgramNode ($1, scope, pType);
			  insertTab (symbolTable, newNode);
			  // comment
			  fprintf(jasmin, "; %s.j\n", fileName);
			  // ---
			  fprintf(jasmin, ".class public %s\n", fileName);
			  fprintf(jasmin, ".super java/lang/Object\n");
			  // for read statement
			  fprintf(jasmin, ".field public static _sc Ljava/util/Scanner;\n");
			  // ---
			  if (strcmp(fileName, $1)) {
				fprintf (stdout, "<Error> found in Line %d: program beginning ID inconsist with file name\n", linenum);
			  }
			}
			  MK_SEMICOLON
			  program_body
			  END ID
			{
			  if (strcmp($1, $6)) {
                 		fprintf (stdout, "<Error> found in Line %d: program end ID inconsist with the beginning ID\n", linenum);
              		  }
			  if (strcmp(fileName, $6)) {
				  fprintf (stdout, "<Error> found in Line %d: program end ID inconsist with file name\n", linenum);
			  }
			  // dump symbol table
			  if( Opt_D == 1 )
				printSymTable( symbolTable, scope );
			}
			;

program_body		: opt_decl_list opt_func_decl_list
			  {
				fprintf(jasmin, "\n.method public static main([Ljava/lang/String;)V\n");
				fprintf(jasmin, ".limit stack 200\n"); 
				fprintf(jasmin, ".limit locals 200\n"); 
				fprintf(jasmin, "new java/util/Scanner\n");
				fprintf(jasmin, "dup\n");
				fprintf(jasmin, "getstatic java/lang/System/in Ljava/io/InputStream;\n");
				fprintf(jasmin, "invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n");
				fprintf(jasmin, "putstatic %s/_sc Ljava/util/Scanner;\n", fileName);
			  }
			  compound_stmt
			  {
				fprintf(jasmin, "return\n");
				fprintf(jasmin, ".end method");
			  }
			;

opt_decl_list		: decl_list
			| /* epsilon */
			;

decl_list		: decl_list decl
			| decl
			;

decl			: VAR id_list MK_COLON scalar_type MK_SEMICOLON       /* scalar type declaration */
			{
			  // insert into symbol table
			  struct idNode_sem *ptr;
			  struct SymNode *newNode;
			  for (ptr=$2 ; ptr!=0; ptr=(ptr->next)) {
			  	if( verifyRedeclaration(symbolTable, ptr->value, scope) == __TRUE ) {
					if(scope==0) // global
					{
						newNode = createVarNode (ptr->value, scope, $4);
						if($4->type==INTEGER_t)
						{
							fprintf(jasmin, ".field public static %s I\n", ptr->value);	
						}
						else if($4->type==REAL_t)
						{
							fprintf(jasmin, ".field public static %s F\n", ptr->value);	
						}
						else if($4->type==BOOLEAN_t)
						{
							fprintf(jasmin, ".field public static %s Z\n", ptr->value);	
						}
					}
					else // local
					{
						
						newNode = createVarNode (ptr->value, scope, $4);
						newNode->varCount=localStack++; // stack position ++
					}
					insertTab (symbolTable, newNode);
				}
			  }
			  
			  deleteIdList( $2 );
			}
			| VAR id_list MK_COLON array_type MK_SEMICOLON        /* array type declaration */
			{
			  verifyArrayType( $2, $4 );
			  // insert into symbol table
			  struct idNode_sem *ptr;
			  struct SymNode *newNode;
			  for( ptr=$2 ; ptr!=0 ; ptr=(ptr->next) ) {
			  	if( $4->isError == __TRUE ) { }
				else if( verifyRedeclaration( symbolTable, ptr->value, scope ) ==__FALSE ) { }
				else {
					newNode = createVarNode( ptr->value, scope, $4 );
					insertTab( symbolTable, newNode );
				}
			  }
			  
			  deleteIdList( $2 );
			}
			| VAR id_list MK_COLON literal_const MK_SEMICOLON     /* const declaration */
			{
			  struct PType *pType = createPType( $4->category );
			  // insert constants into symbol table
			  struct idNode_sem *ptr;
			  struct SymNode *newNode;
			  for( ptr=$2 ; ptr!=0 ; ptr=(ptr->next) ) {
			  	if( verifyRedeclaration( symbolTable, ptr->value, scope ) ==__FALSE ) { }
				else {
					newNode = createConstNode( ptr->value, scope, pType, $4 );
					insertTab( symbolTable, newNode );
				}
			  }
			  
			  deleteIdList( $2 );
			}
			;

literal_const		: INT_CONST
			{
			  int tmp = $1;
			  $$ = createConstAttr( INTEGER_t, &tmp );
			}
			| OP_SUB INT_CONST
			{
			  int tmp = -$2;
			  $$ = createConstAttr( INTEGER_t, &tmp );
			}
			| FLOAT_CONST
			{
			  float tmp = $1;
			  $$ = createConstAttr( REAL_t, &tmp );
			}
			| OP_SUB FLOAT_CONST
			{
			  float tmp = -$2;
			  $$ = createConstAttr( REAL_t, &tmp );
			}
			| SCIENTIFIC 
			{
			  float tmp = $1;
			  $$ = createConstAttr( REAL_t, &tmp );
			}
			| OP_SUB SCIENTIFIC
			{
			  float tmp = -$2;
			  $$ = createConstAttr( REAL_t, &tmp );
			}
			| STR_CONST
			{
			  $$ = createConstAttr( STRING_t, $1 );
			}
			| TRUE
			{
			  __BOOLEAN tmp = __TRUE;
			  $$ = createConstAttr( BOOLEAN_t, &tmp );
			}
			| FALSE
			{
			  __BOOLEAN tmp = __FALSE;
			  $$ = createConstAttr( BOOLEAN_t, &tmp );
			}
			;

opt_func_decl_list	: func_decl_list
			| /* epsilon */
			;

func_decl_list		: func_decl_list func_decl
			| func_decl
			;

func_decl		: ID
	   		{
			  localStack = 0; // for funtion's local variable number (parameters and local variables)
			}
	  		  MK_LPAREN opt_param_list
			{
			  // check and insert parameters into symbol table
			  paramError = insertParamIntoSymTable( symbolTable, $4, scope+1 );
			  // ---
			  struct param_sem *paraPtr = 0;
		  	  for(paraPtr=$4; paraPtr!=0; paraPtr=paraPtr->next)
			  {
				struct idNode_sem *idPtr;
				for(idPtr=paraPtr->idlist; idPtr!=0; idPtr=idPtr->next)
				{		
					struct SymNode *node = 0;
					node = lookupSymbol(symbolTable, idPtr->value, scope+1, __FALSE);
					if(node!=0)
					{
						node->varCount=localStack++;
					}
				}
			  }
			}
			  MK_RPAREN opt_type 
			{
			  // check and insert function into symbol table
			  if( paramError == __TRUE ) {
			  	printf("<Error> found in Line %d: param(s) with several error\n", linenum);
			  } else if( $7->isArray == __TRUE ) {
					printf("<Error> found in Line %d: a function cannot return an array type\n", linenum);
			  } else {
				insertFuncIntoSymTable( symbolTable, $1, $4, $7, scope );
			  }
			  funcReturn = $7;
			  // ---
			  if( paramError == __FALSE )
			  {
				fprintf(jasmin, ".method public static %s(", $1);
				struct param_sem *paraPtr;
				for(paraPtr=$4; paraPtr!=0; paraPtr=paraPtr->next)
				{
					struct idNode_sem *idPtr;
					for(idPtr=paraPtr->idlist; idPtr!=0; idPtr=idPtr->next)
					{
						if(paraPtr->pType->type==INTEGER_t)
						{
							fprintf(jasmin, "I");
						}
						else if(paraPtr->pType->type==REAL_t)
						{
							fprintf(jasmin, "F");
						}
						else if(paraPtr->pType->type==BOOLEAN_t)
						{
							fprintf(jasmin, "Z");
						}
					}
				}
				if($7->type==INTEGER_t)
				{
					fprintf(jasmin, ")I\n");
				}
				else if($7->type==REAL_t)
				{
					fprintf(jasmin, ")F\n");
				}
				else if($7->type==BOOLEAN_t)
				{
					fprintf(jasmin, ")Z\n");
				}
				else
				{
					fprintf(jasmin, ")V\n");
				}
				fprintf(jasmin, ".limit stack 128\n");
				fprintf(jasmin, ".limit locals 128\n");
			  }
			  // ---
			}
			  MK_SEMICOLON
			  compound_stmt
			  END ID
			{
			  // ---
		 	  if($7->type==INTEGER_t)
			  {
				fprintf(jasmin, "ireturn\n");
			  }
			  else if($7->type==REAL_t)
			  {
				fprintf(jasmin, "freturn\n");
			  }
			  else if($7->type==BOOLEAN_t)
			  {
				fprintf(jasmin, "ireturn\n");
			  }
			  else
			  {
		  		fprintf(jasmin, "return\n");
			  }
			  fprintf(jasmin, ".end method\n\n");
			  // ---
			  if( strcmp($1,$12) ) {
				fprintf( stdout, "<Error> found in Line %d: the end of the functionName mismatch\n", linenum );
			  }
			  funcReturn = 0;
			  localStack = 0;
			}
			;

opt_param_list		: param_list 
			  {
				$$ = $1; 
			  }
			| /* epsilon */ { $$ = 0; }
			;

param_list		: param_list MK_SEMICOLON param
			{
			  param_sem_addParam( $1, $3 );
			  $$ = $1;
			}
			| param { $$ = $1; }
			;

param			: id_list MK_COLON type { $$ = createParam( $1, $3 ); }
			;

id_list			: id_list MK_COMMA ID
			{
			  idlist_addNode( $1, $3 );
			  $$ = $1;
			}
			| ID
			  {
				 $$ = createIdList($1); 
			  }
			;

opt_type		: MK_COLON type { $$ = $2; }
			| /* epsilon */ { $$ = createPType( VOID_t ); }
			;

type			: scalar_type { $$ = $1; }
			| array_type { $$ = $1; }
			;

scalar_type		: INTEGER { $$ = createPType (INTEGER_t); }
			| REAL { $$ = createPType (REAL_t); }
			| BOOLEAN { $$ = createPType (BOOLEAN_t); }
			| STRING { $$ = createPType (STRING_t); }
			;

array_type		: ARRAY array_index TO array_index OF type
			{
				verifyArrayDim ($6, $2, $4);
				increaseArrayDim ($6, $2, $4);
				$$ = $6;
			}
			;

array_index		: INT_CONST { $$ = $1; }
			;

stmt			: compound_stmt
			| simple_stmt
			| cond_stmt
			| while_stmt
			| for_stmt
			| return_stmt
			| proc_call_stmt
			;

compound_stmt		: 
			{ 
			  scope++;
			}
			  BEG
			  opt_decl_list
			  opt_stmt_list
			  END 
			{ 
			  // print contents of current scope
			  if( Opt_D == 1 )
			  	printSymTable( symbolTable, scope );
			  deleteScope( symbolTable, scope );	// leave this scope, delete...
			  scope--; 
			}
			;

opt_stmt_list		: stmt_list
			| /* epsilon */
			;

stmt_list		: stmt_list stmt
			| stmt
			;

simple_stmt		: var_ref OP_ASSIGN boolean_expr MK_SEMICOLON
			{
			  // check if LHS exists
			  __BOOLEAN flagLHS = verifyExistence( symbolTable, $1, scope, __TRUE );
			  // id RHS is not dereferenced, check and deference
			  __BOOLEAN flagRHS = __TRUE;
			  if( $3->isDeref == __FALSE ) {
				flagRHS = verifyExistence( symbolTable, $3, scope, __FALSE );
			  }
			  // if both LHS and RHS are exists, verify their type
			  if( flagLHS==__TRUE && flagRHS==__TRUE )
			  {
				verifyAssignmentTypeMatch( $1, $3 );
				struct SymNode *node = 0;
				node = lookupSymbol(symbolTable, $1->varRef->id, scope, __FALSE);
				if(node==0){ }
				else
				{
					if(node->scope!=0) // local
					{
						if(node->category==VARIABLE_t)
						{
							if($1->pType->type==INTEGER_t)
							{
								if($3->pType->type==INTEGER_t)
								{
									fprintf(jasmin, "istore %d\n", node->varCount);	
								}
							}
							else if($1->pType->type==REAL_t)
							{
								if($3->pType->type==INTEGER_t)
								{
									// integer to real
									fprintf(jasmin, "i2f\n");
									fprintf(jasmin, "fstore %d\n", node->varCount);

								}
								else if($3->pType->type==REAL_t)
								{
									fprintf(jasmin, "fstore %d\n", node->varCount);
								}
							}
							else if($1->pType->type==BOOLEAN_t)
							{
								if($3->pType->type==BOOLEAN_t)
								{
									fprintf(jasmin, "istore %d\n", node->varCount);
								}
							}
						}	
					}
					else // global
					{
						if(node->category==VARIABLE_t)
						{
							if($1->pType->type==INTEGER_t)
							{
								if($3->pType->type==INTEGER_t)
								{
									fprintf(jasmin, "putstatic %s/%s I\n", fileName, node->name);	
								}
							}
							else if($1->pType->type==REAL_t)
							{
								if($3->pType->type==INTEGER_t)
								{
									// integer to real
									fprintf(jasmin, "i2f\n");
									fprintf(jasmin, "putstatic %s/%s I\n", fileName, node->name);	
								}
								else if($3->pType->type==REAL_t)
								{
									fprintf(jasmin, "putstatic %s/%s F\n", fileName, node->name);	

								}
							}
							else if($1->pType->type==BOOLEAN_t)
							{
								if($3->pType->type==BOOLEAN_t)
								{
									fprintf(jasmin, "putstatic %s/%s Z\n", fileName, node->name);	
								}
							}
						}	
					}
				}
			  }
			}
			| PRINT
			  {
				fprintf(jasmin, "getstatic java/lang/System/out Ljava/io/PrintStream;\n");
			  }
			  boolean_expr MK_SEMICOLON
			  {
				verifyScalarExpr( $3, "print" );
				if($3->pType->type==STRING_t)
				{
					fprintf(jasmin, "invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
				}
				else if($3->pType->type==INTEGER_t)
				{
					fprintf(jasmin, "invokevirtual java/io/PrintStream/print(I)V\n");
				}
				else if($3->pType->type==REAL_t)
				{
					fprintf(jasmin, "invokevirtual java/io/PrintStream/print(F)V\n");
				}
				else if($3->pType->type==BOOLEAN_t)
				{
					fprintf(jasmin, "invokevirtual java/io/PrintStream/print(Z)V\n");
				}
			  }
 			| READ
			  {
				R=__TRUE;
			  }
			  boolean_expr MK_SEMICOLON
			  {
				fprintf(jasmin, "getstatic %s/_sc Ljava/util/Scanner;\n", fileName);
				
				verifyScalarExpr( $3, "read" );
				struct SymNode *node = 0;
				node=lookupSymbol(symbolTable, $3->varRef->id, scope, __FALSE);
				if(node!=0)
				{
					if(node->category==VARIABLE_t)
					{
						if($3->pType->type==INTEGER_t)
						{
							fprintf(jasmin, "invokevirtual java/util/Scanner/nextInt()I\n");
						} 
						else if($3->pType->type==REAL_t)
						{
							fprintf(jasmin, "invokevirtual java/util/Scanner/nextFloat()F\n");
						}
						else if($3->pType->type==BOOLEAN_t)
						{
							fprintf(jasmin, "invokevirtual java/util/Scanner/nextBoolean()Z\n");
						}

						if(node->category==VARIABLE_t)
						{
							if(node->scope==0)
							{
								// global	
								if($3->pType->type==INTEGER_t)
								{
									fprintf(jasmin, "putstatic %s/%s I\n", fileName, node->name);	
								}
								else if($3->pType->type==REAL_t)
								{
									fprintf(jasmin, "putstatic %s/%s F\n", fileName, node->name);	
								}
								else if($3->pType->type==BOOLEAN_t)
								{
									fprintf(jasmin, "putstatic %s/%s Z\n", fileName, node->name);	
								}
							}
							else
							{	
								// local
								if($3->pType->type==INTEGER_t||$3->pType->type==BOOLEAN_t)
								{
									fprintf(jasmin, "istore %d\n", node->varCount);
								}
								else if($3->pType->type==REAL_t)
								{
									fprintf(jasmin, "fstore %d\n", node->varCount);
								}
							}
						}
					}
				}
				R=__FALSE;
			  }
			;

proc_call_stmt		: ID MK_LPAREN opt_boolean_expr_list MK_RPAREN MK_SEMICOLON
			{
			  verifyFuncInvoke( $1, $3, symbolTable, scope );
			  // ---
			  struct SymNode *_Node = 0;
			  struct expr_sem *exprPtr = 0;
			  _Node = lookupSymbol(symbolTable, $1, 0, __FALSE);
			  if(_Node!=0)
			  {
				struct PTypeList *p = 0;
				for(exprPtr=$3, p=_Node->attribute->formalParam->params; exprPtr!=0 && p!=0; exprPtr=exprPtr->next, p=p->next)
				{
					if(exprPtr->pType->type==INTEGER_t && p->value->type==REAL_t)
					{
						// need type coercion
						struct expr_sem *e = 0;
						for(e=exprPtr->next; e!=0; e=e->next)
						{
							if(e->pType->type==INTEGER_t || e->pType->type==BOOLEAN_t)
							{
								fprintf(jasmin, "istore %d\n", localStack);
								localStack++;
							}
							else if(e->pType->type==REAL_t)
							{
								fprintf(jasmin, "fstore %d\n", localStack);
								localStack++;
							}
						}
						fprintf(jasmin, "i2f\n");

						for(e=exprPtr->next; e!=0; e=e->next)
						{
							if(e->pType->type==INTEGER_t || e->pType->type==BOOLEAN_t)
							{
								localStack--;
								fprintf(jasmin, "iload %d\n", localStack);
							}
							else if(e->pType->type==REAL_t)
							{
								localStack--;
								fprintf(jasmin, "fload %d\n", localStack);
							}
						}
						
					}
				}
			  }
			  // ---
			  fprintf(jasmin, "invokestatic %s/%s(", fileName, $1);
			  struct SymNode *node = 0;
			  node = lookupSymbol(symbolTable, $1, 0, __FALSE);
			  if(node!=0)
			  {
			  	struct PTypeList *paraPtr = 0;
				for(paraPtr=node->attribute->formalParam->params; paraPtr!=0; paraPtr=paraPtr->next)
				{
					if(paraPtr->value->type==INTEGER_t)
					{
						fprintf(jasmin, "I");
					}
					else if(paraPtr->value->type==REAL_t)
					{
						fprintf(jasmin, "F");
					}
					else if(paraPtr->value->type==BOOLEAN_t)
					{
						fprintf(jasmin, "Z");
					}
				}
				if(node->type->type==INTEGER_t)
				{
					fprintf(jasmin, ")I\n");
				}
				else if(node->type->type==REAL_t)
				{
					fprintf(jasmin, ")F\n");
				}
				else if(node->type->type==BOOLEAN_t)
				{
					fprintf(jasmin, ")Z\n");
				}
				else 
				{
					fprintf(jasmin, ")V\n");
				}
			  }
			}
			;

cond_stmt		: IF condition THEN
			  opt_stmt_list
			  {
				fprintf(jasmin, "goto Lexit%d\n", layer[top]);
				fprintf(jasmin, "Lfalse%d:\n", layer[top]);
			  }
			  ELSE
			  opt_stmt_list
			  {
				fprintf(jasmin, "Lexit%d:\n", layer[top]);
				top--;	
			  }
			  END IF
			| IF
			  condition THEN opt_stmt_list
			  {
				fprintf(jasmin, "Lfalse%d:\n", layer[top]);
				top--;
			  }
			  END IF
			;

condition		: boolean_expr 
	   		  {
				top++;
				labelCount++;
				layer[top]=labelCount;
				fprintf(jasmin, "ifeq Lfalse%d\n", layer[top]);
				verifyBooleanExpr( $1, "if" );
			  } 
			;

while_stmt		: WHILE
	    		  {
				top++;
				labelCount++;
				layer[top]=labelCount;
				fprintf(jasmin, "Lbegin%d:\n", layer[top]);
			  }
	   		  condition_while
			  {
				fprintf(jasmin, "ifeq Lexit%d\n", layer[top]);
			  }
			  DO opt_stmt_list
			  {
				fprintf(jasmin, "goto Lbegin%d\n", layer[top]);
				fprintf(jasmin, "Lexit%d:\n", layer[top]);
				top--;
			  }
			  END DO
			;

condition_while		: boolean_expr { verifyBooleanExpr( $1, "while" ); } 
			;

for_stmt		: FOR ID 
			{ 
			  insertLoopVarIntoTable( symbolTable, $2 );
			  struct SymNode *node = 0;
			  node = lookupLoopVar(symbolTable, $2);
			  node->varCount = localStack;
			  localStack++;
			}
			  OP_ASSIGN loop_param TO loop_param
			{
			  verifyLoopParam( $5, $7 );
			  top++;
			  labelCount++;
			  layer[top]=labelCount;
			  struct SymNode *node = 0;
			  node = lookupLoopVar(symbolTable, $2);
			  if(node!=0)
			  {
				fprintf(jasmin, "sipush %d\n", $5);
				fprintf(jasmin, "istore %d\n", node->varCount);
				fprintf(jasmin, "Lbegin%d:\n", layer[top]);
				fprintf(jasmin, "iload %d\n", node->varCount);
				fprintf(jasmin, "sipush %d\n", $7+1);
				fprintf(jasmin, "isub\n");
				fprintf(jasmin, "iflt Ltrue%d\n", layer[top]);
				fprintf(jasmin, "iconst_0\n");
				fprintf(jasmin, "goto Lfalse%d\n", layer[top]);
				fprintf(jasmin, "Ltrue%d:\n", layer[top]);
				fprintf(jasmin, "iconst_1\n");
				fprintf(jasmin, "Lfalse%d:\n", layer[top]);
				fprintf(jasmin, "ifeq Lexit%d\n", layer[top]);
			  }
			}
			  DO 
			  opt_stmt_list 
			  END DO
			{
			  struct SymNode *node = 0;
			  node = lookupLoopVar(symbolTable, $2);
			  if(node!=0)
			  {
				fprintf(jasmin, "iload %d\n", node->varCount);
				fprintf(jasmin, "sipush 1\n");
				fprintf(jasmin, "iadd\n");
				fprintf(jasmin, "istore %d\n", node->varCount);
				fprintf(jasmin, "goto Lbegin%d\n", layer[top]);
				fprintf(jasmin, "Lexit%d:\n", layer[top]);
			  }
			  top--;
			  popLoopVar( symbolTable );
			}
			;

loop_param		: INT_CONST { $$ = $1; }
			| OP_SUB INT_CONST { $$ = -$2; }
			;

return_stmt		: RETURN boolean_expr MK_SEMICOLON
			{
			  verifyReturnStatement( $2, funcReturn );
			}
			;

opt_boolean_expr_list	: boolean_expr_list { $$ = $1; }
			| /* epsilon */ { $$ = 0; }	// null
			;

boolean_expr_list	: boolean_expr_list MK_COMMA boolean_expr
			{
			  struct expr_sem *exprPtr;
			  for( exprPtr=$1 ; (exprPtr->next)!=0 ; exprPtr=(exprPtr->next) );
			  exprPtr->next = $3;
			  $$ = $1;
			}
			| boolean_expr
			{
			  $$ = $1;
			}
			;

boolean_expr		: boolean_expr OP_OR boolean_term
			{
			  verifyAndOrOp( $1, OR_t, $3 );
			  $$ = $1;
			  fprintf(jasmin, "ior\n");
			}
			| boolean_term { $$ = $1; }
			;

boolean_term		: boolean_term OP_AND boolean_factor
			{
			  verifyAndOrOp( $1, AND_t, $3 );
			  $$ = $1;
			  fprintf(jasmin, "iand\n");
			}
			| boolean_factor { $$ = $1; }
			;

boolean_factor		: OP_NOT boolean_factor 
			{
			  verifyUnaryNOT( $2 );
			  $$ = $2;
			  fprintf(jasmin, "iconst_1\n");
			  fprintf(jasmin, "ixor\n");
			}
			| relop_expr { $$ = $1; }
			;

relop_expr		: expr rel_op expr
			{
			  verifyRelOp( $1, $2, $3 );
			  // $1 type has been turn to boolean
			  $$ = $1;
			  // relational op both have the same type----
			  top++;
			  labelCount++;
			  layer[top]=labelCount;
			  if($3->pType->type==INTEGER_t)
			  {
				fprintf(jasmin, "isub\n");
			  }
			  else if($3->pType->type==REAL_t)
			  {
				fprintf(jasmin, "fcmpl\n");
			  }
			  
			  if($2==LT_t) // <
			  {
			  	fprintf(jasmin, "iflt Ltrue%d\n", layer[top]);
			  }
			  else if($2==LE_t) // <=
			  {
			  	fprintf(jasmin, "ifle Ltrue%d\n", layer[top]);
			  }
			  else if($2==EQ_t) // =
			  {
			  	fprintf(jasmin, "ifeq Ltrue%d\n", layer[top]);
			  }
			  else if($2==GE_t) // >=
			  {
			  	fprintf(jasmin, "ifge Ltrue%d\n", layer[top]);
			  }
			  else if($2==GT_t) // >
			  {
			  	fprintf(jasmin, "ifgt Ltrue%d\n", layer[top]);
			  }
			  else if($2==NE_t) // <>
			  {
			  	fprintf(jasmin, "ifne Ltrue%d\n", layer[top]);
			  }
			  fprintf(jasmin, "iconst_0\n");
			  fprintf(jasmin, "goto Lfalse%d\n", layer[top]);
			  fprintf(jasmin, "Ltrue%d:\n", layer[top]);
			  fprintf(jasmin, "iconst_1\n");
			  fprintf(jasmin, "Lfalse%d:\n", layer[top]);
			  top--;
			}
			| expr { $$ = $1; }
			;

rel_op			: OP_LT { $$ = LT_t; }
			| OP_LE { $$ = LE_t; }
			| OP_EQ { $$ = EQ_t; }
			| OP_GE { $$ = GE_t; }
			| OP_GT { $$ = GT_t; }
			| OP_NE { $$ = NE_t; }
			;

expr			: expr add_op term
			{
				if( $2 == ADD_t )
				{
					if($1->pType->type==REAL_t && $3->pType->type==REAL_t)
					{
						fprintf(jasmin, "fadd\n");
					}
					else if($1->pType->type==INTEGER_t && $3->pType->type==REAL_t)
					{
						fprintf(jasmin, "fstore %d\n", localStack);
						fprintf(jasmin, "i2f\n"); // turn integer ( now at stack top ) to real
						fprintf(jasmin, "fload %d\n", localStack);
						fprintf(jasmin, "fadd\n");
					}
					else if($1->pType->type==REAL_t && $3->pType->type==INTEGER_t)
					{	
						fprintf(jasmin, "i2f\n"); // turn integer ( now at stack top ) to real
						fprintf(jasmin, "fadd\n");
					}
					else if($1->pType->type==INTEGER_t && $3->pType->type==INTEGER_t)
					{
						fprintf(jasmin, "iadd\n");
					}
				}
				else if( $2 == SUB_t )
				{
					if($1->pType->type==REAL_t && $3->pType->type==REAL_t)
					{
						fprintf(jasmin, "fsub\n");
					}
					else if($1->pType->type==INTEGER_t && $3->pType->type==REAL_t)
					{
						fprintf(jasmin, "fstore %d\n", localStack);
						fprintf(jasmin, "i2f\n"); // turn integer ( now at stack top ) to real
						fprintf(jasmin, "fload %d\n", localStack);
						fprintf(jasmin, "fsub\n");
					}
					else if($1->pType->type==REAL_t && $3->pType->type==INTEGER_t)
					{
						fprintf(jasmin, "i2f\n");
						fprintf(jasmin, "fsub\n");
					}
					else if($1->pType->type==INTEGER_t && $3->pType->type==INTEGER_t)
					{
						fprintf(jasmin, "isub\n");
					}
				}
				verifyArithmeticOp( $1, $2, $3 );
				$$ = $1;
			}
			| term { $$ = $1; }
			;

add_op			: OP_ADD { $$ = ADD_t; }
			| OP_SUB { $$ = SUB_t; }
			;

term			: term mul_op factor
			{
			  if( $2 == MOD_t ) 
			  {
				verifyModOp( $1, $3 );
				fprintf(jasmin, "irem\n");
			  }
			  else 
			  {
				if( $2 == DIV_t )
				{
					if($1->pType->type==REAL_t && $3->pType->type==REAL_t)
					{
						fprintf(jasmin, "fdiv\n");
					}
					else if($1->pType->type==INTEGER_t && $3->pType->type==REAL_t)
					{
						fprintf(jasmin, "fstore %d\n", localStack);
						fprintf(jasmin, "i2f\n"); // turn integer ( now at stack top ) to real
						fprintf(jasmin, "fload %d\n", localStack);
						fprintf(jasmin, "fdiv\n");
					}
					else if($1->pType->type==REAL_t && $3->pType->type==INTEGER_t)
					{
						fprintf(jasmin, "i2f\n"); // turn integer ( now at stack top ) to real
						fprintf(jasmin, "fdiv\n");
					}
					else if($1->pType->type==INTEGER_t && $3->pType->type==INTEGER_t)
					{
						fprintf(jasmin, "idiv\n");
					}
				}
				else if( $2 == MUL_t )
				{
					if($1->pType->type==REAL_t && $3->pType->type==REAL_t)
					{
						fprintf(jasmin, "fmul\n");
					}
					else if($1->pType->type==INTEGER_t && $3->pType->type==REAL_t)
					{
						fprintf(jasmin, "fstore %d\n", localStack);
						fprintf(jasmin, "i2f\n"); // turn integer ( now at stack top ) to real
						fprintf(jasmin, "fload %d\n", localStack);
						fprintf(jasmin, "fmul\n");
					}
					else if($1->pType->type==REAL_t && $3->pType->type==INTEGER_t)
					{
						fprintf(jasmin, "i2f\n"); // turn integer ( now at stack top ) to real
						fprintf(jasmin, "fmul\n");
					}
					else if($1->pType->type==INTEGER_t && $3->pType->type==INTEGER_t)
					{
						fprintf(jasmin, "imul\n");
					}
				}
			  }
			  verifyArithmeticOp( $1, $2, $3 );
			  $$ = $1;
			}
			| factor { $$ = $1; }
			;

mul_op			: OP_MUL { $$ = MUL_t; }
			| OP_DIV { $$ = DIV_t; }
			| OP_MOD { $$ = MOD_t; }
			;

factor			: var_ref
			{
			  verifyExistence( symbolTable, $1, scope, __FALSE );
			  $$ = $1;
			  $$->beginningOp = NONE_t;

			  struct SymNode *_Node = 0;
			  _Node = lookupLoopVar(symbolTable, $1->varRef->id);
			  struct SymNode *node = 0;
			  node = lookupSymbol(symbolTable, $1->varRef->id, scope, __FALSE);
			  // ---
			  if(_Node!=0)
			  {
				// found loop variable
				fprintf(jasmin, "iload %d\n", _Node->varCount);
			  }
			  else
			  {
				  if(node==0)
				  {
					// not found
			 	  }
			 	  else if(R==__FALSE)
				  {
					if(node->category==CONSTANT_t)
					{
						if(node->type->type==INTEGER_t)
						{
							fprintf(jasmin, "ldc %d\n", node->attribute->constVal->value.integerVal);
						}
						else if(node->type->type==REAL_t)
						{
							fprintf(jasmin, "ldc %f\n", node->attribute->constVal->value.realVal);
						}
						else if(node->type->type==STRING_t)
						{
							fprintf(jasmin, "ldc \"%s\"\n", node->attribute->constVal->value.stringVal);
						}
						else if(node->type->type==BOOLEAN_t)
						{
							if(node->attribute->constVal->value.booleanVal==__TRUE)
							{
								fprintf(jasmin, "iconst_1\n");
							}
							else
							{
								fprintf(jasmin, "iconst_0\n");
							}
						}
					}
					else if(node->category==VARIABLE_t && node->scope==0)
					{
						// global
						if(node->type->type==INTEGER_t)
						{
							fprintf(jasmin, "getstatic %s/%s I\n", fileName, node->name);
						}
						else if(node->type->type==REAL_t)
						{
							fprintf(jasmin, "getstatic %s/%s F\n", fileName, node->name);
						}
						else if(node->type->type==BOOLEAN_t)
						{
							fprintf(jasmin, "getstatic %s/%s Z\n", fileName, node->name);
						}
					}
					else if(node->category==VARIABLE_t && node->scope!=0)
					{
						// local
						if(node->type->type==INTEGER_t || node->type->type==BOOLEAN_t)
						{
							fprintf(jasmin, "iload %d\n", node->varCount);	
						}
						else if(node->type->type==REAL_t)
						{
							fprintf(jasmin, "fload %d\n", node->varCount);
						}
					}
					else if(node->category==PARAMETER_t)
					{
						// function parameter
						if(node->type->type==INTEGER_t || node->type->type==BOOLEAN_t)
						{
							fprintf(jasmin, "iload %d\n", node->varCount);	
						}
						else if(node->type->type==REAL_t)
						{
							fprintf(jasmin, "fload %d\n", node->varCount);
						}
					}
			 	 }
			  }
			  // ---
			}
			| OP_SUB var_ref
			{
			  if( verifyExistence( symbolTable, $2, scope, __FALSE ) == __TRUE )
				verifyUnaryMinus( $2 );
			  $$ = $2;
			  $$->beginningOp = SUB_t;
			  struct SymNode *_Node = 0;
			  _Node = lookupLoopVar(symbolTable, $2->varRef->id);
			  struct SymNode *node = 0;
			  node = lookupSymbol(symbolTable, $2->varRef->id, scope, __FALSE);
			  // ---
			  if(_Node!=0)
			  {
				// found loop variable
				fprintf(jasmin, "iload %d\n", _Node->varCount);
			  }
			  else
			  {
				  if(node==0)
				  {
					// not found
			 	  }
			 	  else if(R==__FALSE)
				  {
					if(node->category==CONSTANT_t)
					{
						if(node->type->type==INTEGER_t)
						{
							fprintf(jasmin, "ldc %d\n", node->attribute->constVal->value.integerVal);
						}
						else if(node->type->type==REAL_t)
						{
							fprintf(jasmin, "ldc %f\n", node->attribute->constVal->value.realVal);
						}
						else if(node->type->type==STRING_t)
						{
							fprintf(jasmin, "ldc \"%s\"\n", node->attribute->constVal->value.stringVal);
						}
						else if(node->type->type==BOOLEAN_t)
						{
							if(node->attribute->constVal->value.booleanVal==__TRUE)
							{
								fprintf(jasmin, "iconst_1\n");
							}
							else
							{
								fprintf(jasmin, "iconst_0\n");
							}
						}
					}
					else if(node->category==VARIABLE_t && node->scope==0)
					{
						// global
						if(node->type->type==INTEGER_t)
						{
							fprintf(jasmin, "getstatic %s/%s I\n", fileName, node->name);
						}
						else if(node->type->type==REAL_t)
						{
							fprintf(jasmin, "getstatic %s/%s F\n", fileName, node->name);
						}
						else if(node->type->type==BOOLEAN_t)
						{
							fprintf(jasmin, "getstatic %s/%s Z\n", fileName, node->name);
						}
					}
					else if(node->category==VARIABLE_t && node->scope!=0)
					{
						// local
						if(node->type->type==INTEGER_t || node->type->type==BOOLEAN_t)
						{
							fprintf(jasmin, "iload %d\n", node->varCount);	
						}
						else if(node->type->type==REAL_t)
						{
							fprintf(jasmin, "fload %d\n", node->varCount);
						}
					}
					else if(node->category==PARAMETER_t)
					{
						// function parameter
						if(node->type->type==INTEGER_t || node->type->type==BOOLEAN_t)
						{
							fprintf(jasmin, "iload %d\n", node->varCount);	
						}
						else if(node->type->type==REAL_t)
						{
							fprintf(jasmin, "fload %d\n", node->varCount);
						}
					}
			 	 }
			  }
			 // deal with negative---
			 if($2->pType->type==INTEGER_t)
			 {
				fprintf(jasmin, "ineg\n");
			 }
			 else if($2->pType->type==REAL_t)
			 {
				fprintf(jasmin, "fneg\n");	
			 }
			}
			| MK_LPAREN boolean_expr MK_RPAREN 
			{
			  $2->beginningOp = NONE_t;
			  $$ = $2; 
			}
			| OP_SUB MK_LPAREN boolean_expr MK_RPAREN
			{
			  verifyUnaryMinus( $3 );
			  $$ = $3;
			  $$->beginningOp = SUB_t;
			  // deal with negative---
			  if($3->pType->type==INTEGER_t)
			  {
				fprintf(jasmin, "ineg\n");
			  }
			  else if($3->pType->type==REAL_t)
			  {
				fprintf(jasmin, "fneg\n");
			  }
			}
			| ID MK_LPAREN opt_boolean_expr_list MK_RPAREN
			{
			  $$ = verifyFuncInvoke( $1, $3, symbolTable, scope );
			  $$->beginningOp = NONE_t;
			  // ---
			  struct SymNode *_Node = 0;
			  struct expr_sem *exprPtr = 0;
			  _Node = lookupSymbol(symbolTable, $1, 0, __FALSE);
			  if(_Node!=0)
			  {
				struct PTypeList *p = 0;
				for(exprPtr=$3, p=_Node->attribute->formalParam->params; exprPtr!=0 && p!=0; exprPtr=exprPtr->next, p=p->next)
				{
					if(exprPtr->pType->type==INTEGER_t && p->value->type==REAL_t)
					{
						// need type coercion
						struct expr_sem *e = 0;
						for(e=exprPtr->next; e!=0; e=e->next)
						{
							if(e->pType->type==INTEGER_t || e->pType->type==BOOLEAN_t)
							{
								fprintf(jasmin, "istore %d\n", localStack);
								localStack++;
							}
							else if(e->pType->type==REAL_t)
							{
								fprintf(jasmin, "fstore %d\n", localStack);
								localStack++;
							}
						}
						fprintf(jasmin, "i2f\n");

						for(e=exprPtr->next; e!=0; e=e->next)
						{
							if(e->pType->type==INTEGER_t || e->pType->type==BOOLEAN_t)
							{
								localStack--;
								fprintf(jasmin, "iload %d\n", localStack);
							}
							else if(e->pType->type==REAL_t)
							{
								localStack--;
								fprintf(jasmin, "fload %d\n", localStack);
							}
						}
					}
				}
			  }
			  // ---
			  fprintf(jasmin, "invokestatic %s/%s(", fileName, $1);
			  struct SymNode *node = 0;
			  node = lookupSymbol(symbolTable, $1, 0, __FALSE);
			  if(node!=0)
			  {
			  	struct PTypeList *paraPtr = 0;
				for(paraPtr=node->attribute->formalParam->params; paraPtr!=0; paraPtr=paraPtr->next)
				{
					if(paraPtr->value->type==INTEGER_t)
					{
						fprintf(jasmin, "I");
					}
					else if(paraPtr->value->type==REAL_t)
					{
						fprintf(jasmin, "F");
					}
					else if(paraPtr->value->type==BOOLEAN_t)
					{
						fprintf(jasmin, "Z");
					}
				}
				if(node->type->type==INTEGER_t)
				{
					fprintf(jasmin, ")I\n");
				}
				else if(node->type->type==REAL_t)
				{
					fprintf(jasmin, ")F\n");
				}
				else if(node->type->type==BOOLEAN_t)
				{
					fprintf(jasmin, ")Z\n");
				}
				else 
				{
					fprintf(jasmin, ")V\n");
				}
			  }
			}
			| OP_SUB ID MK_LPAREN opt_boolean_expr_list MK_RPAREN
			{
			  $$ = verifyFuncInvoke( $2, $4, symbolTable, scope );
			  $$->beginningOp = SUB_t;
			  // ---
			  struct SymNode *_Node = 0;
			  struct expr_sem *exprPtr = 0;
			  _Node = lookupSymbol(symbolTable, $2, 0, __FALSE);
			  if(_Node!=0)
			  {
				struct PTypeList *p = 0;
				for(exprPtr=$4, p=_Node->attribute->formalParam->params; exprPtr!=0 && p!=0; exprPtr=exprPtr->next, p=p->next)
				{
					if(exprPtr->pType->type==INTEGER_t && p->value->type==REAL_t)
					{
						// need type coercion
						struct expr_sem *e = 0;
						for(e=exprPtr->next; e!=0; e=e->next)
						{
							if(e->pType->type==INTEGER_t || e->pType->type==BOOLEAN_t)
							{
								fprintf(jasmin, "istore %d\n", localStack);
								localStack++;
							}
							else if(e->pType->type==REAL_t)
							{
								fprintf(jasmin, "fstore %d\n", localStack);
								localStack++;
							}
						}
						fprintf(jasmin, "i2f\n");

						for(e=exprPtr->next; e!=0; e=e->next)
						{
							if(e->pType->type==INTEGER_t || e->pType->type==BOOLEAN_t)
							{
								localStack--;
								fprintf(jasmin, "iload %d\n", localStack);
							}
							else if(e->pType->type==REAL_t)
							{
								localStack--;
								fprintf(jasmin, "fload %d\n", localStack);
							}
						}
						
					}
				}
			  }
			  // ---
			  fprintf(jasmin, "invokestatic %s/%s(", fileName, $2);
			  struct SymNode *node = 0;
			  node = lookupSymbol(symbolTable, $2, 0, __FALSE);
			  if(node!=0)
			  {
			  	struct PTypeList *paraPtr = 0;
				for(paraPtr=node->attribute->formalParam->params; paraPtr!=0; paraPtr=paraPtr->next)
				{
					if(paraPtr->value->type==INTEGER_t)
					{
						fprintf(jasmin, "I");
					}
					else if(paraPtr->value->type==REAL_t)
					{
						fprintf(jasmin, "F");
					}
					else if(paraPtr->value->type==BOOLEAN_t)
					{
						fprintf(jasmin, "Z");
					}
				}
				if(node->type->type==INTEGER_t)
				{
					fprintf(jasmin, ")I\n");
				}
				else if(node->type->type==REAL_t)
				{
					fprintf(jasmin, ")F\n");
				}
				else if(node->type->type==BOOLEAN_t)
				{
					fprintf(jasmin, ")Z\n");
				}
				else 
				{
					fprintf(jasmin, ")V\n");
				}
			  }
			  // deal with negative ---
			  if(node->type->type==INTEGER_t)
			  {
				fprintf(jasmin, "ineg\n");
			  }
			  else if(node->type->type==REAL_t)
			  {
				fprintf(jasmin, "fneg\n");
			  }
			}
			| literal_const
			{
			  $$ = (struct expr_sem *)malloc(sizeof(struct expr_sem));
			  $$->isDeref = __TRUE;
			  $$->varRef = 0;
			  $$->pType = createPType( $1->category );
			  $$->next = 0;
			  if( $1->hasMinus == __TRUE ) {
			  	$$->beginningOp = SUB_t;
			  }
			  else {
				$$->beginningOp = NONE_t;
			  }
			  // load const to stack
			  if($1->category==STRING_t)
			  {
				fprintf(jasmin, "ldc \"%s\"\n", $1->value.stringVal);	
			  }
			  else if($1->category==BOOLEAN_t)
			  {
				fprintf(jasmin, "iconst_%d\n", $1->value.booleanVal);	
			  }
			  else if($1->category==INTEGER_t)
			  {
				fprintf(jasmin, "ldc %d\n", $1->value.integerVal);	
			  }
			  else if($1->category==REAL_t)
			  {
				fprintf(jasmin, "ldc %f\n", $1->value.realVal);	
			  }
			}
			;

var_ref			: ID
			{
			  $$ = createExprSem( $1 );
			}
			| var_ref dim
			{
			  increaseDim( $1, $2 );
			  $$ = $1;
			}
			;

dim			: MK_LB boolean_expr MK_RB
			{
			  $$ = verifyArrayIndex( $2 );
			}
			;

%%

int yyerror( char *msg )
{
	(void) msg;
	fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
	fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, buf );
	fprintf( stderr, "|\n" );
	fprintf( stderr, "| Unmatched token: %s\n", yytext );
	fprintf( stderr, "|--------------------------------------------------------------------------\n" );
	exit(-1);
}

