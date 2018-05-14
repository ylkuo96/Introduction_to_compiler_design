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

//#include "test.h"

int yydebug;

extern int linenum;		/* declared in lex.l */
extern FILE *yyin;		/* declared by lex */
extern char *yytext;		/* declared by lex */
extern char buf[256];		/* declared in lex.l */
extern int yylex(void);
int yyerror(char* );

int scope = 0;

int Opt_D = 1;			/* symbol table dump option */
char fileName[256];
struct SymTable *symbolTable;	// main symbol table
__BOOLEAN paramError;			// indicate is parameter have any error?
struct PType *funcReturn;		// record function's return type, used at 'return statement' production rule
// -----
bool infunction=false;
bool funcScar=true;
char *temp;
char *simp; // to catch 'char *temp' from variable reference's ID 
int paramCount=0;
int noerror=0;
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

program		: ID
			{
				if(strcmp($1,fileName)!=0)
				{
					printf("<Error> found in Line %d: program beginning ID inconsist with file name\n", linenum);
					noerror=1;
				}			

			  struct PType *pType = createPType( VOID_t );
			  struct SymNode *newNode = createProgramNode( $1, scope, pType );
			  insertTab( symbolTable, newNode );
			}
			  MK_SEMICOLON 
			  program_body
			  END ID
			{
				if(strcmp($1,$6)!=0)
				{ 
					printf("<Error> found in Line %d: program end ID inconsist with the beginning ID\n", linenum);
					noerror=1;
				}	
				if(strcmp($6,fileName)!=0)
				{ 
					printf("<Error> found in Line %d: program end ID inconsist with file name\n", linenum);
					noerror=1;
				}	
				
			  // dump symbol table
			  if( Opt_D == 1 )
				printSymTable( symbolTable, scope );
			}
			;

program_body	: opt_decl_list opt_func_decl_list compound_stmt
			;

opt_decl_list	: decl_list
			| /* epsilon */
			;

decl_list	: decl_list decl
			| decl
			;

decl		: VAR id_list MK_COLON scalar_type MK_SEMICOLON       /* scalar type declaration */
			{
			  // insert into symbol table
			  struct idNode_sem *ptr;
			  struct SymNode *newNode;
			  for( ptr=$2 ; ptr!=0 ; ptr=(ptr->next) )
			  {
			  	if( verifyRedeclaration( symbolTable, ptr->value, scope ) ==__FALSE ) { }
				else 
				{
					newNode = createVarNode( ptr->value, scope, $4 );
					insertTab( symbolTable, newNode );
				}
			  }

			  deleteIdList( $2 );
			}
			| VAR id_list MK_COLON array_type MK_SEMICOLON        /* array type declaration */
			{
			  // insert into symbol table
			  struct idNode_sem *ptr;
			  struct SymNode *newNode;
			  for( ptr=$2 ; ptr!=0 ; ptr=(ptr->next) ) 
			  {
			  	if( $4->isError == __TRUE ) 
				{ 
					// ------------------------------
					//printf("Error at Line#%d: wrong dimension declaration for array ",linenum);
					//printIdList(idlist_buf);
				}
				else if( verifyRedeclaration( symbolTable, ptr->value, scope ) ==__FALSE ) { }
				else 
				{
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
			  for( ptr=$2 ; ptr!=0 ; ptr=(ptr->next) ) 
			  {
			  	if( verifyRedeclaration( symbolTable, ptr->value, scope ) ==__FALSE ) { }
				else 
				{
					newNode = createConstNode( ptr->value, scope, pType, $4 );
					insertTab( symbolTable, newNode );
				}
			  }
			  
			  deleteIdList( $2 );
			}
			;

literal_const	: INT_CONST
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

func_decl	: ID MK_LPAREN opt_param_list
			{
			  infunction=true;
			  // check and insert parameters into symbol table
			  paramError = insertParamIntoSymTable( symbolTable, $3, scope+1 );
			  funcScar=true;
			}
			  MK_RPAREN opt_type 
			{
			  if(funcScar)
			  {
				// check and insert function into symbol table
				insertFuncIntoSymTable( symbolTable, $1, $3, $6, scope );
			  }
			  funcReturn = $6;
			}
			  MK_SEMICOLON 
			{
				if(!funcScar)
				{
					printf("<Error> found in Line %d: function cannot return an array type\n", linenum);
					noerror=1;
				}
				funcScar=true;
			}
			  compound_stmt
			  END ID
			{	
				infunction=false;
				if(strcmp($1,$12)!=0)
				{ 
					printf("<Error> found in Line %d: Function end ID inconsist with the beginning ID\n", linenum);
					noerror=1;
				}
			  funcReturn = 0;
			}
			;

opt_param_list	: param_list { $$ = $1; }
			| /* epsilon */ { $$ = 0; }
			;

param_list	: param_list MK_SEMICOLON param
			{
			  param_sem_addParam( $1, $3 );
			  $$ = $1;
			}
			| param { $$ = $1; }
			;

param		: id_list MK_COLON type { $$ = createParam( $1, $3 ); }
			;

id_list		: id_list MK_COMMA ID
			{
			  idlist_addNode( $1, $3 );
			  $$ = $1;
			}
			| ID { $$ = createIdList($1); }
			;

opt_type	: MK_COLON type { $$ = $2; }
			| /* epsilon */ { $$ = createPType( VOID_t ); }
			;

type		: scalar_type { $$ = $1; }
			| array_type { $$ = $1; }
			;

scalar_type	: INTEGER { $$ = createPType( INTEGER_t ); }
			| REAL { $$ = createPType( REAL_t ); }
			| BOOLEAN { $$ = createPType( BOOLEAN_t ); }
			| STRING { $$ = createPType( STRING_t ); }
			;

array_type	: ARRAY array_index TO array_index OF type
			{
				if($2>=$4 || $2<0 || $4<0)
				{
					printf("<Error> found in Line %d: array indexes are not legal\n", linenum);
					noerror=1;
				}
				funcScar=false; // set function return type
				
				increaseArrayDim( $6, $2, $4 );
				$$ = $6;
			}
			;

array_index	: INT_CONST { $$ = $1; }
			;

stmt		: compound_stmt
			| simple_stmt
			| cond_stmt
			| while_stmt
			| for_stmt
			| return_stmt 
			| proc_call_stmt
			;

compound_stmt	: 
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

opt_stmt_list	: stmt_list 
			| /* epsilon */
			;

stmt_list	: stmt_list stmt 
			| stmt 
			;

simple_stmt	: var_ref { simp = temp; } OP_ASSIGN boolean_expr MK_SEMICOLON
			{
				struct SymNode *node = 0;
                node = lookupSymbol( symbolTable, simp, scope, __FALSE );
				
				// for forloop variable
				struct SymNode *node1 = 0;
				node1 = lookupLoopVar( symbolTable, simp );
				if(node1!=0)
				{
					if(node1->category==LOOPVAR_t)
					{
						printf("<Error> found in Line %d: loop variable can not be assigned\n", linenum);
						noerror=1;
					}
				}		
				else if(node==0)
				{
					printf("<Error> found in Line %d: identifier '%s' is not declared\n",linenum, simp);
					//printf("1\n");
					noerror=1;
			    }
				else 
				{
					if(node->category==CONSTANT_t)
					{
						printf("<Error> found in Line %d: constant can not be assigned\n", linenum);
						noerror=1;
					}

					if($1->pType==0 || $4->pType==0)
					{
						if($1->pType==0 && $4->pType!=0)
						{
							printf("<Error> found in Line %d: type mismatch\n", linenum);
							noerror=1;
						}
						else if($1->pType!=0 && $4->pType==0)
						{
							printf("<Error> found in Line %d: type mismatch\n", linenum);
							noerror=1;
						}
					}
					else if($1->pType->type!=$4->pType->type)
					{
						if($1->pType->type==REAL_t && $4->pType->type==INTEGER_t)
						{
							// -------------------------------------------------			
							if($1->pType->isArray && $4->pType->isArray)
							{
								if($1->pType->dimNum != $1->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
									noerror=1;
								}
								else if($4->pType->dimNum != $4->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
									noerror=1;
								}
								
								if(($1->pType->dimNum - $1->varRef->dimNum)!=($4->pType->dimNum - $4->varRef->dimNum))
								{
									// array dimension is not the same
									printf("<Error> found in Line %d: type mismatch, array dimension is not the same\n", linenum);
									noerror=1;
									//L: $1->pType->dimNum-$1->varRef->dimNum ,R: $4->pType->dimNum-$4->varRef->dimNum
								}
								else
								{
									// both have the same dimension, so check the rest array dimension
									struct ArrayDimNode *lPtr; // assignment left for $1
									struct ArrayDimNode *rPtr; // assignment right for $4
									int i=0;
									for(i=0, lPtr=($1->pType->dim) ; i<$1->varRef->dimNum ; i++, lPtr=lPtr->next ); // go to the location not used
									for(i=0, rPtr=($4->pType->dim) ; i<$4->varRef->dimNum ; i++, rPtr=rPtr->next ); // go to the location not used
									for(;lPtr!=0;lPtr=lPtr->next)
									{
										if(lPtr->size!=rPtr->size)
										{
											printf("<Error> found in Line %d: type mismatch, array dimension size is not the same\n", linenum);
											noerror=1;
											break;
										}
										
										if(lPtr->next!=0 && rPtr->next==0)
										{
											printf("<Error> found in Line %d: type mismatch, array dimension is not the same\n", linenum);
											noerror=1;
											break;										
										}
										else if(lPtr->next==0 && rPtr->next!=0)
										{
											printf("<Error> found in Line %d: type mismatch, array dimension is not the same\n", linenum);
											noerror=1;
											break;										
										}
										else if(lPtr->next==0 && rPtr->next==0)
										{
											// both array type are the same!
										}
										
										if(rPtr->next!=0)
										{
											rPtr=rPtr->next;
										}
									}
								}
							}
							else if($1->pType->isArray)
							{
								if($1->pType->dimNum != $1->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
									printf("<Error> found in Line %d: type mismatch\n", linenum);
									noerror=1;
								}	
							}
							else if($4->pType->isArray)
							{
								if($4->pType->dimNum != $4->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
									printf("<Error> found in Line %d: type mismatch\n", linenum);
									noerror=1;
								}	
							}
							else
							{
								// ok!
							}
							// -------------------------------------------------			
						}
						else
						{
							printf("<Error> found in Line %d: type mismatch\n", linenum);
							noerror=1;
						}
					}
					else
					{
						// type == type
						// check array dimension -------------------------------------------------------------
						if($1->pType==0 || $4->pType==0)
						{
							if($1->pType==0 && $4->pType!=0)
							{
								printf("<Error> found in Line %d: type mismatch\n", linenum);
								noerror=1;
							}
							else if($1->pType!=0 && $4->pType==0)
							{
								printf("<Error> found in Line %d: type mismatch\n", linenum);
								noerror=1;
							}
						}
						else if($1->pType->isArray && $4->pType->isArray)
						{
							if($1->pType->dimNum != $1->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
								noerror=1;
							}
							else if($4->pType->dimNum != $4->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
								noerror=1;
							}
							
							if(($1->pType->dimNum - $1->varRef->dimNum)!=($4->pType->dimNum - $4->varRef->dimNum))
							{
								// array dimension is not the same, ex 2D vs 3D array
								printf("<Error> found in Line %d: type mismatch, array dimension is not the same\n", linenum);
								noerror=1;
								//L: $1->pType->dimNum-$1->varRef->dimNum ,R: $4->pType->dimNum-$4->varRef->dimNum
							}
							else
							{
								// both have the same dimension, so check the rest array dimension
								struct ArrayDimNode *lPtr; // assignment left for $1
								struct ArrayDimNode *rPtr; // assignment right for $4
								int i=0;
								for(i=0, lPtr=($1->pType->dim) ; i<$1->varRef->dimNum ; i++, lPtr=lPtr->next ); // go to the location not used
								for(i=0, rPtr=($4->pType->dim) ; i<$4->varRef->dimNum ; i++, rPtr=rPtr->next ); // go to the location not used
								for(;lPtr!=0;lPtr=lPtr->next)
								{
									if(lPtr->size!=rPtr->size)
									{
										printf("<Error> found in Line %d: type mismatch, array dimension size is not the same\n", linenum);
										noerror=1;
										break;
									}
									
									if(lPtr->next!=0 && rPtr->next==0)
									{
										printf("<Error> found in Line %d: type mismatch, array dimension is not the same\n", linenum);
										noerror=1;
										break;										
									}
									else if(lPtr->next==0 && rPtr->next!=0)
									{
										printf("<Error> found in Line %d: type mismatch, array dimension is not the same\n", linenum);
										noerror=1;
										break;										
									}
									else if(lPtr->next==0 && rPtr->next==0)
									{
										// both array type are the same!
									}
									
									if(rPtr->next!=0)
									{
										rPtr=rPtr->next;
									}
								}
							}
						}
						else if($1->pType->isArray)
						{
							if($1->pType->dimNum != $1->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
								printf("<Error> found in Line %d: type mismatch\n", linenum);
								noerror=1;
							}	
						}
						else if($4->pType->isArray)
						{
							if($4->pType->dimNum != $4->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
								printf("<Error> found in Line %d: type mismatch\n", linenum);
								noerror=1;
							}	
						}
					}
				}
			}
			| PRINT boolean_expr MK_SEMICOLON
			{
				if($2->pType->isArray)
				{
					if($2->pType->dimNum!=$2->varRef->dimNum)
					{
						printf("<Error> found in Line %d: print statement should be scalar type\n", linenum);
						noerror=1;
					}
				}
			}
 			| READ boolean_expr MK_SEMICOLON
			{
				if($2->pType->isArray)
				{
					if($2->pType->dimNum!=$2->varRef->dimNum)
					{
						printf("<Error> found in Line %d: read statement should be scalar type\n", linenum);
						noerror=1;
					}
				}
			}
			;

proc_call_stmt	: ID MK_LPAREN opt_boolean_expr_list MK_RPAREN MK_SEMICOLON
			{
			  //verifyFuncInvoke( $1, $3, symbolTable, scope );

              struct SymNode *node = 0;
              node = lookupSymbol( symbolTable, $1, scope, __FALSE );

			  if(node==0)
			  {
				printf("<Error> found in Line %d: identifier '%s' is not declared\n", linenum, $1);
				//printf("2\n");
				noerror=1;
			  }
			  else if(node->category!=FUNCTION_t)
			  {
				printf("<Error> found in Line %d: identifier '%s' is not function\n", linenum, $1);
				//printf("%d\n",scope);
				noerror=1;
			  }
			  else
			  {
				// check parameter num 
				if(node->attribute->formalParam->paramNum!=paramCount)
				{
					printf("<Error> found in Line %d: parameter number inconsistent\n",linenum);
					noerror=1;
				}
				else
				{
					// check parameter type
					struct expr_sem *exprPtr;
					struct PTypeList *paraPtr=node->attribute->formalParam->params;
					bool checked=false;
					if(paraPtr!=0)
					{
						if($3!=0)
						{
							for(exprPtr=$3, paraPtr=node->attribute->formalParam->params; exprPtr!=0 && paraPtr!=0; exprPtr=exprPtr->next, paraPtr=paraPtr->next)
							{
								// compare each parameter term 
								if(exprPtr->pType->type!=paraPtr->value->type)
								{
									// check array type
									if(exprPtr->pType->type==INTEGER_t && paraPtr->value->type==REAL_t)
									{
										// --------------------------------------------------
										if(exprPtr->pType->isArray && paraPtr->value->isArray)
										{
											if(exprPtr->varRef!=0)
											{
												struct ArrayDimNode *e;
												struct ArrayDimNode *p;
												int i;
												for(e=exprPtr->pType->dim, i=exprPtr->varRef->dimNum; i!=0; i--, e=e->next);
												for(p=paraPtr->value->dim; p!=0; p=p->next)
												{
													if(e->size!=p->size)
													{
														printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
														noerror=1;
														break;
													}
													
													if(e->next!=0 && p->next==0)
													{
														printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
														noerror=1;
														break;										
													}
													else if(e->next==0 && p->next!=0)
													{
														printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
														noerror=1;
														break;										
													}
													else if(e->next==0 && p->next==0)
													{
														// both array type are the same!
													}
													
													if(e->next!=0)
													{
														e=e->next;
													}
												}
											}
										}
										else if(exprPtr->pType->isArray)
										{
											if(exprPtr->varRef!=0)
											{
												if(exprPtr->pType->dimNum!=exprPtr->varRef->dimNum)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;
												}
											}
										}
										else if(paraPtr->value->isArray)
										{
											printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
											noerror=1;
											break;
										}
										// --------------------------------------------------
									}
									else
									{
										printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
										noerror=1;
										break;
									}
								}
								else
								{
									if(exprPtr->pType->isArray && paraPtr->value->isArray)
									{
										if(exprPtr->varRef!=0)
										{
											struct ArrayDimNode *e;
											struct ArrayDimNode *p;
											int i;
											for(e=exprPtr->pType->dim, i=exprPtr->varRef->dimNum; i!=0; i--, e=e->next);
											for(p=paraPtr->value->dim; p!=0; p=p->next)
											{
												if(e->size!=p->size)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;
												}
												
												if(e->next!=0 && p->next==0)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;										
												}
												else if(e->next==0 && p->next!=0)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;										
												}
												else if(e->next==0 && p->next==0)
												{
													// both array type are the same!
												}
												
												if(e->next!=0)
												{
													e=e->next;
												}
											}
										}
									}
									else if(exprPtr->pType->isArray)
									{
										if(exprPtr->varRef!=0)
										{
											if(exprPtr->pType->dimNum!=exprPtr->varRef->dimNum)
											{
												printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
												noerror=1;
												break;
											}
										}
									}
									else if(paraPtr->value->isArray)
									{
										printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
										noerror=1;
										break;
									}										
								}
							}
						}
					}
					else if($3!=0)
					{
						printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
						noerror=1;
					}
				}
			  }
			  // reset paramater count
			  paramCount=0; 
			}
			;

cond_stmt	: IF condition THEN
			  opt_stmt_list
			  ELSE
			  opt_stmt_list
			  END IF
			| IF condition THEN opt_stmt_list END IF
			;

condition	: boolean_expr
			{
				if($1->pType==0)
				{
					printf("<Error> found in Line %d: condition expression should be boolean\n",linenum);
					noerror=1;
				}
				else if($1->pType->isArray && ($1->pType->dimNum != $1->varRef->dimNum))
				{
					printf("<Error> found in Line %d: condition expression should not be array type\n",linenum);
					noerror=1;					
				}
				else if($1->pType->type!=BOOLEAN_t)
				{
					printf("<Error> found in Line %d: condition expression should be boolean\n",linenum);
					noerror=1;
				}
			}
			;

while_stmt	: WHILE condition_while DO
			  opt_stmt_list
			  END DO
			;

condition_while	: boolean_expr
			{
				if($1->pType==0)
				{
					printf("<Error> found in Line %d: condition expression should be boolean\n",linenum);
					noerror=1;
				}
				else if($1->pType->isArray && ($1->pType->dimNum != $1->varRef->dimNum))
				{
					printf("<Error> found in Line %d: condition expression should not be array type\n",linenum);
					noerror=1;					
				}
				else if($1->pType->type!=BOOLEAN_t)
				{
					printf("<Error> found in Line %d: condition expression should be boolean\n",linenum);
					noerror=1;
				}		
			}
			;

for_stmt	: FOR ID
			{
			  insertLoopVarIntoTable( symbolTable, $2 );
			}
			  OP_ASSIGN loop_param TO loop_param DO
			{
				if($5>$7 || $5<0 || $7<0)
				{
					printf("<Error> found in Line %d: for loop's parameters are illegal\n",linenum);
					noerror=1;
				}
			}
			  opt_stmt_list
			  END DO
			{
			  popLoopVar( symbolTable );
			}
			;

loop_param	: INT_CONST { $$ = $1; }
			| OP_SUB INT_CONST { $$ = -$2; }
			;

return_stmt	: RETURN boolean_expr MK_SEMICOLON 
			  {
				if(!infunction)
				{
					printf("<Error> found in Line %d: Program can not be returned\n",linenum);
					noerror=1;
				}
				else // function
				{
					if($2->pType==0 || funcReturn->type==0)
					{
						if($2->pType ==0 && funcReturn->type==0)
						{
							// ok!
						}
						else
						{
							printf("<Error> found in Line %d: return type mismatch\n", linenum);
							noerror=1;
						}
					}
					else if($2->pType->type!=funcReturn->type)
					{
						printf("<Error> found in Line %d: return type mismatch with function declaration\n",linenum);
						noerror=1;
					}
					else // same scalar type
					{
						if($2->pType->isArray && !funcReturn->isArray)
						{
							if($2->pType->dimNum!=$2->varRef->dimNum)
							{
								printf("<Error> found in Line %d: return type mismatch with function declaration\n",linenum);
								noerror=1;
							}
						}
						else if(!$2->pType->isArray && funcReturn->isArray)
						{
							printf("<Error> found in Line %d: return type mismatch with function declaration\n",linenum);
							noerror=1;
						}
						else if($2->pType->isArray && funcReturn->isArray)
						{
							if($2->pType->dimNum!=$2->varRef->dimNum)
							{
								printf("<Error> found in Line %d: function cannot return an array type\n",linenum);
								noerror=1;
							}
							else // scalar vs array
							{
								printf("<Error> found in Line %d: return type mismatch with function declaration\n",linenum);
								noerror=1;							
							}
						}
					}
				}
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
			  paramCount++;
			}
			| boolean_expr
			{
			  $$ = $1;
			  paramCount++;
			}
			;

boolean_expr: boolean_expr OP_OR boolean_term
			{
			  $$ = $1;
			  if($1->pType==0)
			  {
				//
			  }
			  else if($1->pType->type!=BOOLEAN_t) 
			  {
				printf("<Error> found in Line %d: operands between 'or' are not boolean\n", linenum);
				noerror=1;
				// give wrong type! $1
		   	  }
			  else if($3->pType==0)
			  {
				//
			  }
			  else if($3->pType->type!=BOOLEAN_t)
			  {
				printf("<Error> found in Line %d: operands between 'or' are not boolean\n", linenum);
				noerror=1;
				// give wrong type!
				$$ = $3;
			  }
			  else if($1->pType->type==BOOLEAN_t && $3->pType->type==BOOLEAN_t)
			  {
				if($1->pType->isArray && ($1->pType->dimNum!=$1->varRef->dimNum))
				{
					printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
					noerror=1;
				}
				else if($3->pType->isArray && ($3->pType->dimNum!=$3->varRef->dimNum))
				{
					printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
					noerror=1;
				}			  
			  }
			}
			| boolean_term { $$ = $1; }
			;

boolean_term: boolean_term OP_AND boolean_factor
			{
			  $$ = $1;
			  if($1->pType==0)
			  {
				//
			  }
			  else if($1->pType->type!=BOOLEAN_t) 
			  {
				printf("<Error> found in Line %d: operands between 'and' are not boolean\n", linenum);
				noerror=1;
				// give wrong type! $1
		   	  }
			  else if($3->pType==0)
			  {
				//
			  }
			  else if($3->pType->type!=BOOLEAN_t)
			  {
				printf("<Error> found in Line %d: operands between 'and' are not boolean\n", linenum);
				noerror=1;
				// give wrong type!
				$$ = $3;
			  }
			  else if($1->pType->type==BOOLEAN_t && $3->pType->type==BOOLEAN_t)
			  {
				if($1->pType->isArray && ($1->pType->dimNum!=$1->varRef->dimNum))
				{
					printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
					noerror=1;
				}
				else if($3->pType->isArray && ($3->pType->dimNum!=$3->varRef->dimNum))
				{
					printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
					noerror=1;
				}			  
			  }
			}
			| boolean_factor { $$ = $1; }
			;

boolean_factor	: OP_NOT boolean_factor 
			{
			  $$ = $2; 
			  if($2->pType==0)
			  {
				//
			  }
			  else if($2->pType->type!=BOOLEAN_t) 
			  {
				printf("<Error> found in Line %d: operand after 'not' is not boolean\n", linenum);
				noerror=1;
		   	  } 	
			  else if($2->pType->type==BOOLEAN_t) 
			  {
				if($2->pType->isArray && ($2->pType->dimNum != $2->varRef->dimNum))
				{
					printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
					noerror=1;
				}
			  }
			}
			| relop_expr { $$ = $1; }
			;

relop_expr	: expr rel_op expr
			{
			  // $$ = $1;
			  if($3->pType==0)
			  {
				$$ = $3;
			  }
			  else if($1->pType==0)
			  {
				$$ = $1;
			  }
			  else if($1->pType!=0 && $3->pType!=0)
			  {
				$$ = (struct expr_sem *)malloc(sizeof(struct expr_sem));
				$$->pType = createPType(BOOLEAN_t);
				$$->isDeref = $1->isDeref;
				$$->varRef = $1->varRef;
				$$->next = $1->next; // 0
				$$->beginningOp = $1->beginningOp;
				if($1->pType->type==INTEGER_t && $3->pType->type==INTEGER_t)
				{
					// legal but check array type
					if($1->pType->isArray && ($1->pType->dimNum!=$1->varRef->dimNum))
					{
						printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
						noerror=1;
					}
					else if($3->pType->isArray && ($3->pType->dimNum!=$3->varRef->dimNum))
					{
						printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
						noerror=1;
					}
				}
				else if($1->pType->type==REAL_t && $3->pType->type==REAL_t)
				{
					// legal but check array type
					if($1->pType->isArray && ($1->pType->dimNum!=$1->varRef->dimNum))
					{
						printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
						noerror=1;
					}
					else if($3->pType->isArray && ($3->pType->dimNum!=$3->varRef->dimNum))
					{
						printf("<Error> found in Line %d: array arithmetic is not allowed\n", linenum);
						noerror=1;
					}
				}
				else
				{
					printf("<Error> found in Line %d: type between rel_op should be both integer or real\n", linenum);
					noerror=1;
				}
			  }
			}
			| expr { $$ = $1; }
			;

rel_op		: OP_LT { $$ = LT_t; }
			| OP_LE { $$ = LE_t; }
			| OP_EQ { $$ = EQ_t; }
			| OP_GE { $$ = GE_t; }
			| OP_GT { $$ = GT_t; }
			| OP_NE { $$ = NE_t; }
			;

expr		: expr add_op term
			{
				if($1->pType==0 || $3->pType==0)
				{
					//printf("<Error> found in Line %d: type mismatch\n", linenum);
					if($1->pType==0 && $3->pType!=0)
					{
						printf("<Error> found in Line %d: type mismatch\n", linenum);
						noerror=1;
					}
					else if($1->pType!=0 && $3->pType==0)
					{
						printf("<Error> found in Line %d: type mismatch\n", linenum);
						noerror=1;
					}
				}
				else
				{
					$$ = (struct expr_sem *)malloc(sizeof(struct expr_sem));
					$$->pType = createPType($1->pType->type);
					$$->isDeref = $1->isDeref;
					$$->varRef = $1->varRef;
					$$->next = $1->next; // 0
					$$->beginningOp = $1->beginningOp;
					if(!$1->pType->isArray && !$3->pType->isArray)
					{
						if($1->pType->type==STRING_t && $3->pType->type==STRING_t) 
						{
							if($2!=ADD_t) // SUB_t
							{
								// print error
								printf("<Error> found in Line %d: between 'sub' can not be string\n", linenum);
								noerror=1;
							}
							else
							{
								// correct ! return type is string
							}
						} 
						else if(($1->pType->type!=REAL_t && $1->pType->type!=INTEGER_t)||
								($3->pType->type!=REAL_t && $3->pType->type!=INTEGER_t ))
						{
							printf("<Error> found in Line %d: between 'add/sub' are not integer/real\n", linenum);
							noerror=1;
						}
						else
						{
							// correct ! set return type
							// type coercion
							if($1->pType->type==REAL_t || $3->pType->type==REAL_t)
							{
								$$->pType = createPType(REAL_t);
							}
						}				
					}
					else if($1->pType->isArray && $3->pType->isArray) // both are array
					{
						if($1->pType->type==STRING_t && $3->pType->type==STRING_t)
						{
							if($2 != ADD_t)
							{
								printf("<Error> found in Line %d: between 'sub' can not be string\n", linenum);
								noerror=1;
							}
							else if($1->pType->dimNum != $1->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed between string concatenation\n", linenum);
								noerror=1;
							}
							else if($3->pType->dimNum != $3->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed between string concatenation\n", linenum);
								noerror=1;
							}
							else
							{
								// correct
							}
						}
						else if(($1->pType->type!=REAL_t && $1->pType->type!=INTEGER_t)||
								($3->pType->type!=REAL_t && $3->pType->type!=INTEGER_t ))
						{
							printf("<Error> found in Line %d: between 'add/sub' are not integer/real\n", linenum);
							noerror=1;
						}
						else
						{
							// check dimension
							if($1->pType->dimNum != $1->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed between 'add/sub'\n", linenum);
								noerror=1;
							}
							else if($3->pType->dimNum != $3->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed between 'add/sub'\n", linenum);
								noerror=1;
							}
							else if($1->pType->type==REAL_t || $3->pType->type==REAL_t)
							{
								// correct ! set return type
								// type coercion
								$$->pType = createPType(REAL_t);
							}
						}					
					}
					else if($1->pType->isArray)
					{
						if($1->pType->type==STRING_t && $3->pType->type==STRING_t)
						{
							if($2 != ADD_t)
							{
								printf("<Error> found in Line %d: between 'sub' can not be string\n", linenum);
								noerror=1;
							}
							else if($1->pType->dimNum != $1->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed between string concatenation\n", linenum);
								noerror=1;
							}
							else
							{
								// correct
							}
						}
						else if(($1->pType->type!=REAL_t && $1->pType->type!=INTEGER_t)||
								($3->pType->type!=REAL_t && $3->pType->type!=INTEGER_t ))
						{
							printf("<Error> found in Line %d: between 'add/sub' are not integer/real\n", linenum);
							noerror=1;
						}
						else
						{
							// check dimension
							if($1->pType->dimNum != $1->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed between 'add/sub'\n", linenum);
								noerror=1;
							}
							else if($1->pType->type==REAL_t || $3->pType->type==REAL_t)
							{
								// correct ! set return type
								// type coercion
								$$->pType = createPType(REAL_t);
							}					
						}
					}
					else if($3->pType->isArray)
					{
						if($1->pType->type==STRING_t && $3->pType->type==STRING_t)
						{
							if($2 != ADD_t)
							{
								printf("<Error> found in Line %d: between 'sub' can not be string\n", linenum);
								noerror=1;
							}
							else if($3->pType->dimNum != $3->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed between string concatenation\n", linenum);
								noerror=1;
							}
							else
							{
								// correct
							}
						}
						else if(($1->pType->type!=REAL_t && $1->pType->type!=INTEGER_t)||
								($3->pType->type!=REAL_t && $3->pType->type!=INTEGER_t ))
						{
							printf("<Error> found in Line %d: between 'add/sub' are not integer/real\n", linenum);
							noerror=1;
						}
						else
						{
							// check dimension
							if($3->pType->dimNum != $3->varRef->dimNum)
							{
								printf("<Error> found in Line %d: array arithmetic is not allowed between 'add/sub'\n", linenum);
								noerror=1;
							}
							else if($1->pType->type==REAL_t || $3->pType->type==REAL_t)
							{
								// correct ! set return type
								// type coercion
								$$->pType = createPType(REAL_t);
							}
						}
					}
				}
			}
			| term { $$ = $1; }
			;

add_op		: OP_ADD { $$ = ADD_t; }
			| OP_SUB { $$ = SUB_t; }
			;

term		: term mul_op factor
			{
				if($1->pType==0 || $3->pType==0)
				{
					//printf("<Error> found in Line %d: type mismatch\n", linenum);
					if($1->pType==0 && $3->pType!=0)
					{
						printf("<Error> found in Line %d: type mismatch\n", linenum);
						noerror=1;
					}
					else if($1->pType!=0 && $3->pType==0)
					{
						printf("<Error> found in Line %d: type mismatch\n", linenum);
						noerror=1;
					}
				}
				else
				{
					$$ = (struct expr_sem *)malloc(sizeof(struct expr_sem));
					$$->pType = createPType($1->pType->type);
					$$->isDeref = $1->isDeref;
					$$->varRef = $1->varRef;
					$$->next = $1->next; // 0
					$$->beginningOp = $1->beginningOp;
					if(!$1->pType->isArray && !$3->pType->isArray)
					{
						if($2==MUL_t || $2==DIV_t)
						{
							if(($1->pType->type!=REAL_t && $1->pType->type!=INTEGER_t)||
							   ($3->pType->type!=REAL_t && $3->pType->type!=INTEGER_t))
							{
								printf("<Error> found in Line %d: between 'mul/div' are not integer/real\n", linenum);
								noerror=1;
							}
							else 
							{
								// correct ! set return type
								// type coercion
								if($1->pType->type==REAL_t || $3->pType->type==REAL_t)
								{
									$$->pType = createPType(REAL_t);
								}
							}
						}
						else if($2==MOD_t)
						{
							if($1->pType->type!=INTEGER_t || $3->pType->type!=INTEGER_t)
							{
								printf("<Error> found in Line %d: between 'mod' are not integer\n", linenum);
								noerror=1;
							}
							else 
							{
								// correct ! return type is integer	
							}
						}				
					}
					else if($1->pType->isArray && $3->pType->isArray) // both are array
					{
						if($2==MUL_t || $2==DIV_t)
						{
							if(($1->pType->type!=REAL_t && $1->pType->type!=INTEGER_t)||
							   ($3->pType->type!=REAL_t && $3->pType->type!=INTEGER_t))
							{
								printf("<Error> found in Line %d: between 'mul/div' are not integer/real\n", linenum);
								noerror=1;
							}
							else 
							{
								if($1->pType->dimNum != $1->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed between 'mul/div'\n", linenum);
									noerror=1;
								}
								else if($3->pType->dimNum != $3->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed between 'mul/div'\n", linenum);
									noerror=1;
								}
								else if($1->pType->type==REAL_t || $3->pType->type==REAL_t)
								{
									// correct ! set return type
									// type coercion
									$$->pType = createPType(REAL_t);
								}
							}
						}
						else if($2==MOD_t)
						{
							if($1->pType->type!=INTEGER_t || $3->pType->type!=INTEGER_t)
							{
								printf("<Error> found in Line %d: between 'mod' are not integer\n", linenum);
								noerror=1;
							}
							else 
							{
								// both are integer, check array dimension
								if($1->pType->dimNum != $1->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed between 'mod'\n", linenum);
									noerror=1;
								}
								else if($3->pType->dimNum != $3->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed between 'mod'\n", linenum);
									noerror=1;
								}
								else
								{
									// correct ! return type is integer already, no need to set
								}
							}
						}								
					}
					else if($1->pType->isArray)
					{
						if($2==MUL_t || $2==DIV_t)
						{
							if(($1->pType->type!=REAL_t && $1->pType->type!=INTEGER_t)||
							   ($3->pType->type!=REAL_t && $3->pType->type!=INTEGER_t))
							{
								printf("<Error> found in Line %d: between 'mul/div' are not integer/real\n", linenum);
								noerror=1;
							}
							else 
							{
								if($1->pType->dimNum != $1->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed between 'mul/div'\n", linenum);
									noerror=1;
								}
								else if($1->pType->type==REAL_t || $3->pType->type==REAL_t)
								{
									// correct ! set return type
									// type coercion
									$$->pType = createPType(REAL_t);
								}
							}
						}
						else if($2==MOD_t)
						{
							if($1->pType->type!=INTEGER_t || $3->pType->type!=INTEGER_t)
							{
								printf("<Error> found in Line %d: between 'mod' are not integer\n", linenum);
								noerror=1;
							}
							else 
							{ 	// both are integer, check array dimension
								if($1->pType->dimNum != $1->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed between 'mod'\n", linenum);
									noerror=1;
								}
								else
								{
									// correct ! return type is integer already, no need to set
								}
							}
						}			
					}
					else if($3->pType->isArray)
					{
						if($2==MUL_t || $2==DIV_t)
						{
							if(($1->pType->type!=REAL_t && $1->pType->type!=INTEGER_t)||
							   ($3->pType->type!=REAL_t && $3->pType->type!=INTEGER_t))
							{
								printf("<Error> found in Line %d: between 'mul/div' are not integer/real\n", linenum);
								noerror=1;
							}
							else 
							{
								if($3->pType->dimNum != $3->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed between 'mul/div'\n", linenum);
									noerror=1;
								}
								else if($1->pType->type==REAL_t || $3->pType->type==REAL_t)
								{
									// correct ! set return type
									// type coercion
									$$->pType = createPType(REAL_t);
								}
							}
						}
						else if($2==MOD_t)
						{
							if($1->pType->type!=INTEGER_t || $3->pType->type!=INTEGER_t)
							{
								printf("<Error> found in Line %d: between 'mod' are not integer\n", linenum);
								noerror=1;
							}
							else 
							{
								// both are integer, check array dimension
								if($3->pType->dimNum != $3->varRef->dimNum)
								{
									printf("<Error> found in Line %d: array arithmetic is not allowed between 'mod'\n", linenum);
									noerror=1;
								}
								else
								{
									// correct ! return type is integer already, no need to set
								}
							}
						}
					}					
				}
			}
			| factor { $$ = $1; }
			;

mul_op		: OP_MUL { $$ = MUL_t; }
			| OP_DIV { $$ = DIV_t; }
			| OP_MOD { $$ = MOD_t; }
			;

factor		: var_ref
			{
			  $$ = $1;
			  $$->beginningOp = NONE_t;
			  if($1->pType==0)
			  {
				//
			  }
			  else if(!$1->pType->isArray && $1->varRef->dimNum != 0 )
			  {
				printf("<Error> found in Line %d: '%s' is not an array type.\n", linenum, $1->varRef->id);
				noerror=1;
			  }
			  else if($1->pType->isArray && ($1->varRef->dimNum > $1->pType->dimNum))
			  {
				printf("<Error> found in Line %d: '%s' is %d dimension(s), but reference in %d dimension(s)\n", linenum, $1->varRef->id, $1->pType->dimNum, $1->varRef->dimNum);
				noerror=1;
			  }
			}
			| OP_SUB var_ref
			{
			  $$ = $2;
			  $$->beginningOp = NONE_t;
			  if($2->pType==0)
			  {
				//
			  }
			  else if(!$2->pType->isArray && $2->varRef->dimNum != 0 )
			  {
				printf("<Error> found in Line %d: '%s' is not an array type.\n", linenum, $2->varRef->id);
				noerror=1;
			  }
			  else if($2->pType->isArray && ($2->varRef->dimNum > $2->pType->dimNum))
			  {
				printf("<Error> found in Line %d: '%s' is %d dimension(s), but reference in %d dimension(s)\n", linenum, $2->varRef->id, $2->pType->dimNum, $2->varRef->dimNum);
				noerror=1;
			  }
			}
			| MK_LPAREN boolean_expr MK_RPAREN 
			{
			  $2->beginningOp = NONE_t;
			  $$ = $2; 
			}
			| OP_SUB MK_LPAREN boolean_expr MK_RPAREN
			{
			  $$ = $3;
			  $$->beginningOp = SUB_t;
			}
			| ID MK_LPAREN opt_boolean_expr_list MK_RPAREN
			{
			  //$$ = verifyFuncInvoke( $1, $3, symbolTable, scope );
			  $$ = (struct expr_sem *)malloc(sizeof(struct expr_sem));
			  $$->isDeref = __TRUE;
			  //$$->varRef = 0;
			  
			  $$->varRef = (struct var_ref_sem *)malloc(sizeof(struct var_ref_sem));
			  $$->varRef->id = (char *)malloc(sizeof(char)*(strlen($1)+1));
			  strcpy( $$->varRef->id, $1 );
			  $$->varRef->dimNum = 0;
			  $$->varRef->dim = 0;
			  
              $$->next = 0;

              struct SymNode *node = 0;
              node = lookupSymbol( symbolTable, $1, 0, __FALSE ); // function is global 0
			  //$$->pType = node->type;
			  $$->beginningOp = NONE_t;
			  
			  // check id is declared
			  if(node==0)
			  {
				printf("<Error> found in Line %d: identifier '%s' is not declared\n", linenum, $1);
				//printf("3\n");
				noerror=1;
			  }
			  else if(node->category!=FUNCTION_t)
			  {
				$$->pType = node->type;
				printf("<Error> found in Line %d: identifier '%s' is not function\n", linenum, $1);
				noerror=1;
			  }
			  else
			  {
				// found
			    $$->pType = node->type;
				//printf("%d", node->attribute->formalParam->paramNum);
				//printf(" ~ ");
				//printf("%d", paramCount);
				//printf("\n");
				
				// check parameter num 
				if(node->attribute->formalParam->paramNum!=paramCount)
				{
					printf("<Error> found in Line %d: parameter number inconsistent\n",linenum);
					noerror=1;
				}
				else
				{
					// check parameter type
					struct expr_sem *exprPtr;
					struct PTypeList *paraPtr=node->attribute->formalParam->params;
					bool checked=false;
					if(paraPtr!=0)
					{
						if($3!=0)
						{
							for(exprPtr=$3, paraPtr=node->attribute->formalParam->params; exprPtr!=0 && paraPtr!=0; exprPtr=exprPtr->next, paraPtr=paraPtr->next)
							{
								// compare each parameter term 
								if(exprPtr->pType->type!=paraPtr->value->type)
								{
									if(exprPtr->pType->type==INTEGER_t && paraPtr->value->type==REAL_t)
									{
										// --------------------------------------------------
										if(exprPtr->pType->isArray && paraPtr->value->isArray)
										{
											if(exprPtr->varRef!=0)
											{
												struct ArrayDimNode *e;
												struct ArrayDimNode *p;
												int i;
												for(e=exprPtr->pType->dim, i=exprPtr->varRef->dimNum; i!=0; i--, e=e->next);
												for(p=paraPtr->value->dim; p!=0; p=p->next)
												{
													if(e->size!=p->size)
													{
														printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
														noerror=1;
														break;
													}
													
													if(e->next!=0 && p->next==0)
													{
														printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
														noerror=1;
														break;										
													}
													else if(e->next==0 && p->next!=0)
													{
														printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
														noerror=1;
														break;										
													}
													else if(e->next==0 && p->next==0)
													{
														// both array type are the same!
													}
													
													if(e->next!=0)
													{
														e=e->next;
													}
												}
											}
										}
										else if(exprPtr->pType->isArray)
										{
											if(exprPtr->varRef!=0)
											{
												if(exprPtr->pType->dimNum!=exprPtr->varRef->dimNum)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;
												}
											}
										}
										else if(paraPtr->value->isArray)
										{
											printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
											noerror=1;
											break;
										}
										// --------------------------------------------------
									}
									else
									{
										printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
										noerror=1;
										break;
									}
								}
								else
								{
									if(exprPtr->pType->isArray && paraPtr->value->isArray)
									{
										if(exprPtr->varRef!=0)
										{
											struct ArrayDimNode *e;
											struct ArrayDimNode *p;
											int i;
											for(e=exprPtr->pType->dim, i=exprPtr->varRef->dimNum; i!=0; i--, e=e->next);
											for(p=paraPtr->value->dim; p!=0; p=p->next)
											{
												if(e->size!=p->size)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;
												}
												
												if(e->next!=0 && p->next==0)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;										
												}
												else if(e->next==0 && p->next!=0)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;										
												}
												else if(e->next==0 && p->next==0)
												{
													// both array type are the same!
												}
												
												if(e->next!=0)
												{
													e=e->next;
												}
											}
										}
									}
									else if(exprPtr->pType->isArray)
									{
										if(exprPtr->varRef!=0)
										{
											if(exprPtr->pType->dimNum!=exprPtr->varRef->dimNum)
											{
												printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
												noerror=1;
												break;
											}
										}
									}
									else if(paraPtr->value->isArray)
									{
										printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
										noerror=1;
										break;
									}								
								}
							}
						}
					}
					else if($3!=0)
					{
						printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
						noerror=1;
					}
				}
			  }
			  // reset paramater count
			  paramCount=0;
			}
			| OP_SUB ID MK_LPAREN opt_boolean_expr_list MK_RPAREN
			{
			  //$$ = verifyFuncInvoke( $2, $4, symbolTable, scope );
			  $$ = (struct expr_sem *)malloc(sizeof(struct expr_sem));
			  $$->isDeref = __TRUE;
			  $$->varRef = 0;
              $$->next = 0;

              struct SymNode *node = 0;
              node = lookupSymbol( symbolTable, $2, 0, __FALSE ); // function is global 0
              //$$->pType = node->type;
			  $$->beginningOp = SUB_t;
			  
			  // check id is declared
			  if(node==0)
			  {
				printf("<Error> found in Line %d: identifier '%s' is not declared\n", linenum, $2);
				//printf("4\n");
				noerror=1;
			  }
			  else if(node->category!=FUNCTION_t)
			  {
				$$->pType = node->type;
				printf("<Error> found in Line %d: identifier '%s' is not function\n", linenum, $2);
				noerror=1;
			  }
			  else
			  {
				// found
			    $$->pType = node->type;
				//printf("%d", node->attribute->formalParam->paramNum);
				//printf(" ~ ");
				//printf("%d", paramCount);
				//printf("\n");
				
				// check parameter num 
				if(node->attribute->formalParam->paramNum!=paramCount)
				{
					printf("<Error> found in Line %d: parameter number inconsistent\n",linenum);
					noerror=1;
				}
				else
				{
					// check parameter type
					struct expr_sem *exprPtr;
					struct PTypeList *paraPtr=node->attribute->formalParam->params;
					bool checked=false;
					if(paraPtr!=0)
					{
						if($4!=0)
						{
							for(exprPtr=$4, paraPtr=node->attribute->formalParam->params; exprPtr!=0 && paraPtr!=0; exprPtr=exprPtr->next, paraPtr=paraPtr->next)
							{
								// compare each parameter term 
								if(exprPtr->pType->type!=paraPtr->value->type)
								{
									if(exprPtr->pType->type==INTEGER_t && paraPtr->value->type==REAL_t)
									{
										// --------------------------------------------------
										if(exprPtr->pType->isArray && paraPtr->value->isArray)
										{
											if(exprPtr->varRef!=0)
											{
												struct ArrayDimNode *e;
												struct ArrayDimNode *p;
												int i;
												for(e=exprPtr->pType->dim, i=exprPtr->varRef->dimNum; i!=0; i--, e=e->next);
												for(p=paraPtr->value->dim; p!=0; p=p->next)
												{
													//printf("%d: %d, %d\n", linenum, e->size, p->size);
													if(e->size!=p->size)
													{
														printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
														noerror=1;
														break;
													}
													
													if(e->next!=0 && p->next==0)
													{
														printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
														noerror=1;
														break;										
													}
													else if(e->next==0 && p->next!=0)
													{
														printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
														noerror=1;
														break;										
													}
													else if(e->next==0 && p->next==0)
													{
														// both array type are the same!
													}
													
													if(e->next!=0)
													{
														e=e->next;
													}
												}
											}
										}
										else if(exprPtr->pType->isArray)
										{
											if(exprPtr->varRef!=0)
											{
												if(exprPtr->pType->dimNum!=exprPtr->varRef->dimNum)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;
												}
											}
										}
										else if(paraPtr->value->isArray)
										{
											printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
											noerror=1;
											break;
										}
										// --------------------------------------------------
									}
									else
									{
										printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
										noerror=1;
										break;
									}
								}
								else
								{
									if(exprPtr->pType->isArray && paraPtr->value->isArray)
									{
										if(exprPtr->varRef!=0)
										{
											struct ArrayDimNode *e;
											struct ArrayDimNode *p;
											int i;
											for(e=exprPtr->pType->dim, i=exprPtr->varRef->dimNum; i!=0; i--, e=e->next);
											for(p=paraPtr->value->dim; p!=0; p=p->next)
											{
												//printf("%d: %d, %d\n", linenum, e->size, p->size);
												if(e->size!=p->size)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;
												}
												
												if(e->next!=0 && p->next==0)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;										
												}
												else if(e->next==0 && p->next!=0)
												{
													printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
													noerror=1;
													break;										
												}
												else if(e->next==0 && p->next==0)
												{
													// both array type are the same!
												}
												
												if(e->next!=0)
												{
													e=e->next;
												}
											}
										}
									}
									else if(exprPtr->pType->isArray)
									{
										if(exprPtr->varRef!=0)
										{
											if(exprPtr->pType->dimNum!=exprPtr->varRef->dimNum)
											{
												printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
												noerror=1;
												break;
											}
										}
									}
									else if(paraPtr->value->isArray)
									{
										printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
										noerror=1;
										break;
									}								
								}
							}
						}
					}
					else if($4!=0) // parameter should not throw things 
					{
						printf("<Error> found in Line %d: parameter type mismatch\n", linenum);
						noerror=1;
					}
				}
			  }
			  // reset paramater count
			  paramCount=0;
			}
			| literal_const
			{
			  $$ = (struct expr_sem *)malloc(sizeof(struct expr_sem));
			  $$->isDeref = __TRUE;
			  $$->varRef = 0;
			  $$->pType = createPType( $1->category );
			  $$->next = 0;
			  if( $1->hasMinus == __TRUE ) 
			  {
			  	$$->beginningOp = SUB_t;
			  }
			  else 
			  {
				$$->beginningOp = NONE_t;
			  }
			}
			;

var_ref		: ID
			{
			  temp = $1;
			  $$ = createExprSem( $1 );
			  struct SymNode *node = 0;
              node = lookupSymbol( symbolTable, $1, scope, __FALSE );
			  if(node==0)
			  {
				// for loop variable
				struct SymNode *node1 = 0;
				node1 = lookupLoopVar( symbolTable, $1 );
				if(node1==0)
				{
					printf("<Error> found in Line %d: identifier '%s' is not declared\n", linenum, $1);
					//printf("5\n");
					noerror=1;
				}
				else
				{
					$$->pType = createPType(INTEGER_t);
				}
			  }
			  else // found
			  {
				$$->pType = node->type;
			  }
			}
			| var_ref dim
			{
			  $$ = $1;
			  increaseDim( $1, $2 );
			}
			;

dim			: MK_LB boolean_expr MK_RB
            {
              $$ = INTEGER_t;
			  
			  if($2->pType->isArray)
			  {
				printf("<Error> found in Line %d: dimension should not be array type\n",linenum);
				noerror=1;
			  }
			  else if($2->pType->type!=INTEGER_t)
			  {
				printf("<Error> found in Line %d: dimension should be integer type\n",linenum);
				noerror=1;
			  }
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

