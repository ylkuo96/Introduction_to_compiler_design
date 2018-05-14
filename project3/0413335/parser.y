%{
/**
 * Introduction to Compiler Design by Prof. Yi Ping You
 * Project 2 YACC sample
 */
#include <stdio.h>
#include <stdlib.h>
#include "symbol.h"
#include <string.h>

using namespace std;

extern int linenum;		    /* declared in lex.l */
extern "C" FILE *yyin;		/* declared by lex */
extern char *yytext;		/* declared by lex */
extern char buf[256];		/* declared in lex.l */
extern "C" int yylex(void);
int yyerror(char* );

vector<symbolTable> tables;
int current=0;
bool build=false;
vector<int> temp;
string TYPE;
string ATTR;
vector<string> temp_s;
vector<vector<int> > temp_d;
string TE; // for function return type
vector<string> forID;
%}

%union{
	char* txt;
	//char* str;
}

/* tokens */
%token ARRAY
%token BEG
%token <txt> BOOLEAN
%token DEF
%token DO
%token ELSE
%token END
%token <txt> FALSE
%token FOR
%token <txt> INTEGER
%token IF
%token OF
%token PRINT
%token READ
%token <txt> REAL
%token RETURN
%token <txt> STRING
%token THEN
%token TO
%token <txt> TRUE
%token VAR
%token WHILE

%token <txt> ID
%token <txt> OCTAL_CONST
%token <txt> INT_CONST
%token <txt> FLOAT_CONST
%token <txt> SCIENTIFIC
%token <txt> STR_CONST

%token OP_ADD
%token OP_SUB
%token OP_MUL
%token OP_DIV
%token OP_MOD
%token OP_ASSIGN
%token OP_EQ
%token OP_NE
%token OP_GT
%token OP_LT
%token OP_GE
%token OP_LE
%token OP_AND
%token OP_OR
%token OP_NOT

%token MK_COMMA
%token MK_COLON
%token MK_SEMICOLON
%token MK_LPAREN
%token MK_RPAREN
%token MK_LB
%token MK_RB

%type <txt> scalar_type
%type <txt> array_type
%type <txt> int_const
%type <txt> type
%type <txt> literal_const
%type <txt> func_decl
%type <txt> opt_type

/* start symbol */
%start program
%%

/**/
program		: ID {
				/*create a symbol table*/
				symbolTable T;
				entry E;
				E.name=$1;
				E.kind="program";
				E.level=current;
				E.type="void";
				string S;
				E.attr.push_back(S);
				
				/*push entry*/
				T.tableEntry.push_back(E);
				T.level=current;
				
				tables.push_back(T);
			} 
			MK_SEMICOLON 
			program_body
			END ID {
				/*pop symbol table and print */
				symbolTable T=tables.back();
				dumpsymbol(T);
				tables.pop_back();	
				current--;				
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

decl		: VAR id_list MK_COLON scalar_type { /* scalar type declaration */
				for(int i=0;i<tables.back().tableEntry.size();i++){
					if(tables.back().tableEntry[i].type=="NO"){
						string tempaa=$4;
						tables.back().tableEntry[i].type=tempaa;
						// type_dim is 0 
					}
				}
				// ----
				bool Del=false;
				int length=tables.back().tableEntry.size();
				for(int i=0;i<length-1;i++){
					if(tables.back().tableEntry[length-1].name==tables.back().tableEntry[i].name){
						printf("<Error> found in Line %d: symbol %s is redeclared\n", linenum, tables.back().tableEntry[length-1].name.c_str());
						Del=true;
						break;
					}
				}
				for(int i=0;i<tables.size();i++){
					if(tables[i].tableEntry.size()>0){
						if(tables[i].tableEntry[0].kind=="for"){
							if(tables.back().tableEntry[length-1].name==tables[i].tableEntry[0].name){
								printf("<Error> found in Line %d: symbol %s is redeclared\n", linenum, tables.back().tableEntry[length-1].name.c_str());
								Del=true;
								break;
							}
						}
					}
				}		
				if(Del){
					tables.back().tableEntry.pop_back();
				}
			} MK_SEMICOLON 
			| VAR id_list MK_COLON array_type { /* array type declaration */
				for(int i=0;i<tables.back().tableEntry.size();i++){
					if(tables.back().tableEntry[i].type=="NO"){
						string tempaa=$4;
						tables.back().tableEntry[i].type=tempaa;
						for(int j=temp.size()-1;j>=0;j--){
							tables.back().tableEntry[i].type_dim.push_back(temp[j]);
						}
					}					
				}
				temp.clear();
				// ----
				bool Del=false;
				int length=tables.back().tableEntry.size();
				for(int i=0;i<length-1;i++){
					if(tables.back().tableEntry[length-1].name==tables.back().tableEntry[i].name){
						printf("<Error> found in Line %d: symbol %s is redeclared\n", linenum, tables.back().tableEntry[length-1].name.c_str());
						Del=true;
						break;
					}
				}
				for(int i=0;i<tables.size();i++){
					if(tables[i].tableEntry.size()>0){
						if(tables[i].tableEntry[0].kind=="for"){
							if(tables.back().tableEntry[length-1].name==tables[i].tableEntry[0].name){
								printf("<Error> found in Line %d: symbol %s is redeclared\n", linenum, tables.back().tableEntry[length-1].name.c_str());
								Del=true;
								break;
							}
						}
					}
				}		
				if(Del){
					tables.back().tableEntry.pop_back();
				}
			} MK_SEMICOLON 
			| VAR id_list MK_COLON literal_const { /* const declaration */
				for(int i=0;i<tables.back().tableEntry.size();i++){
					if(tables.back().tableEntry[i].type=="NO"){
						tables.back().tableEntry[i].type=TYPE;
						tables.back().tableEntry[i].kind="constant";
						tables.back().tableEntry[i].attr.push_back(ATTR);
						// attr_dim is 0, so don't push it, the size is zero
					}				
				}	
				// ----
				bool Del=false;
				int length=tables.back().tableEntry.size();
				for(int i=0;i<length-1;i++){
					if(tables.back().tableEntry[length-1].name==tables.back().tableEntry[i].name){
						printf("<Error> found in Line %d: symbol %s is redeclared\n", linenum, tables.back().tableEntry[length-1].name.c_str());
						Del=true;
						break;
					}
				}
				for(int i=0;i<tables.size();i++){
					if(tables[i].tableEntry.size()>0){
						if(tables[i].tableEntry[0].kind=="for"){
							if(tables.back().tableEntry[length-1].name==tables[i].tableEntry[0].name){
								printf("<Error> found in Line %d: symbol %s is redeclared\n", linenum, tables.back().tableEntry[length-1].name.c_str());
								Del=true;
								break;
							}
						}
					}
				}		
				if(Del){
					tables.back().tableEntry.pop_back();
				}
			} MK_SEMICOLON 
			;
int_const	:	INT_CONST
			|	OCTAL_CONST
			;

literal_const	: int_const {ATTR.assign($1); TYPE="integer";}
			| OP_SUB int_const {string t; t.assign($2); ATTR="-"+t; TYPE="integer";} 
			| FLOAT_CONST {ATTR.assign($1); TYPE="real";}
			| OP_SUB FLOAT_CONST {string t; t.assign($2); ATTR="-"+t; TYPE="real";} 
			| SCIENTIFIC {ATTR.assign($1); TYPE="real";}
			| OP_SUB SCIENTIFIC {string t; t.assign($2); ATTR="-"+t; TYPE="real";} 
			| STR_CONST {string t=$1; ATTR='\"'+t+'\"'; TYPE="string";}
			| TRUE {ATTR="true"; TYPE="boolean";}
			| FALSE {ATTR="false"; TYPE="boolean";}
			;

opt_func_decl_list	: func_decl_list
			| /* epsilon */
			;

func_decl_list	: func_decl_list func_decl
			| func_decl
			;
			
/**/
func_decl  : ID {
				// ----
				string Y=$1;
				int length=tables.back().tableEntry.size();
				for(int i=0;i<length;i++){
					if(Y==tables.back().tableEntry[i].name){
						printf("<Error> found in Line %d: symbol %s is redeclared\n", linenum, Y.c_str());
						break;
					}
				}			
				// ---	
				/*create a new symbol table*/
					symbolTable T;
					T.level=++current;
					tables.push_back(T);
					build=true;		
				} 
				MK_LPAREN opt_param_list 	
				MK_RPAREN opt_type {TE=$6;}
				MK_SEMICOLON 
				compound_stmt
				END ID {
				    /*push entry to the program symbol table*/
					entry E;
					string tempaa=$1;
					E.name=tempaa;
					E.kind="function";
					E.level=current;
					string tempbb=TE;
					E.type=tempbb; // scalar type	
					if(temp.size()>0){ // it has dimension! so it's array
						for(int j=temp.size()-1;j>=0;j--){
							E.type_dim.push_back(temp[j]);
						}					
					}
					temp.clear();
					
					if(temp_s.size()>0){
						for(int j=temp_s.size()-1;j>=0;j--){
							E.attr.push_back(temp_s[j]);
						}
					}
					temp_s.clear();
					
					if(temp_d.size()>0){
						for(int j=temp_d.size()-1;j>=0;j--){
							E.attr_dim.push_back(temp_d[j]);
						}				
					}
					temp_d.clear();
					
					tables.back().tableEntry.push_back(E);	
					// ----
					bool Del=false;
					int length=tables.back().tableEntry.size();
					for(int i=0;i<length-1;i++){
						if(tables.back().tableEntry[length-1].name==tables.back().tableEntry[i].name){
							//printf("<Error> found in Line %d: symbol %s is redeclared\n", linenum, tables.back().tableEntry[length-1].name.c_str());
							Del=true;
							break;
						}
					}
					if(Del){
						tables.back().tableEntry.pop_back();
					}			
					// ---					
				}
			;

opt_param_list	: param_list
			| /* epsilon */
			;

param_list	: param_list MK_SEMICOLON param 
			| param
			;

param		: id_list MK_COLON type {
				int count=0;
				for(int i=0;i<tables.back().tableEntry.size();i++){
					if(tables.back().tableEntry[i].type=="NO"){
						count++;
						string tempaa=$3;
						tables.back().tableEntry[i].type=tempaa;
						tables.back().tableEntry[i].kind="parameter";
						// type_dim is 0 
						if(!temp.empty()){
							for(int j=temp.size()-1;j>=0;j--){
								tables.back().tableEntry[i].type_dim.push_back(temp[j]);
							}					
						}					
						temp_s.push_back($3);
						
						if(temp.size()>0){
							vector<int> A;
							for(int i=temp.size()-1;i>=0;i--){
								A.push_back(temp[i]);
							}
							temp_d.push_back(A);
							temp.clear();
						}
						else {
							vector<int> A;
							temp_d.push_back(A); // empty
						}		
					}
				}
				// ----
				bool Del=false;
				int length=tables.back().tableEntry.size();
				for(int i=0;i<length-1;i++){
					if(tables.back().tableEntry[length-1].name==tables.back().tableEntry[i].name){
						printf("<Error> found in Line %d: symbol %s is redeclared\n", linenum, tables.back().tableEntry[length-1].name.c_str());
						Del=true;
						break;
					}
				}
				if(Del){
					tables.back().tableEntry.pop_back();
				}					
				else{/*
					temp_s.push_back($3);
					
					if(temp.size()>0){
						vector<int> A;
						for(int i=temp.size()-1;i>=0;i--){
							A.push_back(temp[i]);
						}
						temp_d.push_back(A);
						temp.clear();
					}
					else {
						vector<int> A;
						temp_d.push_back(A); // empty
					}*/
				}
				// ---					
			}
			;

id_list		: id_list MK_COMMA ID {
				entry E;
				string tempaa=$3;
				E.name=tempaa;
				E.kind="variable";
				E.level=current;
				E.type="NO"; 
				
				tables.back().tableEntry.push_back(E);				
			}
			| ID {
				entry E;
				string tempaa=$1;
				E.name=tempaa;
				E.kind="variable";
				E.level=current;
				E.type="NO"; 

				tables.back().tableEntry.push_back(E);
			}
			;

opt_type	: MK_COLON type {$$=$2;}
			| /* epsilon */ {$$="void";}
			;

type		: scalar_type {$$=$1;} 
			| array_type {$$=$1;}
			;

scalar_type	: INTEGER {char t[]="integer"; $$=t;}
			| REAL {char t[]="real"; $$=t;}
			| BOOLEAN {char t[]="boolean"; $$=t;}
			| STRING {char t[]="string"; $$=t;}
			;

array_type	: ARRAY int_const TO int_const OF type  {string a1, a2; a1.assign($4); a2.assign($2); temp.push_back(atoi(a1.c_str())-atoi(a2.c_str())+1); $$=$6;}
			;

stmt		: compound_stmt
			| simple_stmt
			| cond_stmt
			| while_stmt
			| for_stmt
			| return_stmt
			| proc_call_stmt
			;
/**/
compound_stmt	: BEG {
					if(!build){ // need to create a symbol table
						symbolTable T;
						T.level=++current;
						tables.push_back(T);
						build=false;
					}
					else{ // function already build it!
						build=false;
					}
				}
				opt_decl_list 
				opt_stmt_list {
					/*pop symbol table and print */
					symbolTable T=tables.back();
					dumpsymbol(T);
					tables.pop_back(); 
					current--;
				}
				END
			;

opt_stmt_list	: stmt_list
			| /* epsilon */
			;

stmt_list	: stmt_list stmt
			| stmt
			;

simple_stmt	: var_ref OP_ASSIGN boolean_expr MK_SEMICOLON
			| PRINT boolean_expr MK_SEMICOLON
			| READ boolean_expr MK_SEMICOLON
			;

proc_call_stmt	: ID MK_LPAREN opt_boolean_expr_list MK_RPAREN MK_SEMICOLON
			;

cond_stmt	: IF boolean_expr THEN
			  opt_stmt_list
			  ELSE
			  opt_stmt_list
			  END IF
			| IF boolean_expr THEN opt_stmt_list END IF
			;

while_stmt	: WHILE boolean_expr DO
			  opt_stmt_list
			  END DO
			;

for_stmt	: FOR ID {
				symbolTable T;
				T.level=++current;
				tables.push_back(T);	
				entry E;
				E.name=$2;
				forID.push_back(E.name);
				bool Del=false;
				for(int i=0;i<forID.size()-1;i++){
					if(E.name==forID[i]){
						printf("<Error> found in Line %d: symbol %s is redeclared\n", linenum, E.name.c_str());			
						Del=true;
						break;
					}
				}
				if(Del){
					forID.pop_back();
				}
				E.kind="for";
				tables.back().tableEntry.push_back(E);
			  }
			  OP_ASSIGN int_const TO int_const DO
			  opt_stmt_list
			  END DO {
				if(forID.size()>0){
					forID.pop_back();
				}
				tables.pop_back();
				current--;
			  }
			;

return_stmt	: RETURN boolean_expr MK_SEMICOLON
			;

opt_boolean_expr_list	: boolean_expr_list
			| /* epsilon */
			;

boolean_expr_list	: boolean_expr_list MK_COMMA boolean_expr
			| boolean_expr
			;

boolean_expr	: boolean_expr OP_OR boolean_term
			| boolean_term
			;

boolean_term	: boolean_term OP_AND boolean_factor
			| boolean_factor
			;

boolean_factor	: OP_NOT boolean_factor 
			| relop_expr
			;

relop_expr	: expr rel_op expr
			| expr
			;

rel_op		: OP_LT
			| OP_LE
			| OP_EQ
			| OP_GE
			| OP_GT
			| OP_NE
			;

expr		: expr add_op term
			| term
			;

add_op		: OP_ADD
			| OP_SUB
			;

term		: term mul_op factor
			| factor
			;

mul_op		: OP_MUL
			| OP_DIV
			| OP_MOD
			;

factor		: var_ref
			| OP_SUB var_ref
			| MK_LPAREN boolean_expr MK_RPAREN
			| OP_SUB MK_LPAREN boolean_expr MK_RPAREN
			| ID MK_LPAREN opt_boolean_expr_list MK_RPAREN
			| OP_SUB ID MK_LPAREN opt_boolean_expr_list MK_RPAREN
			| literal_const
			;

var_ref		: ID
			| var_ref dim
			;

dim			: MK_LB boolean_expr MK_RB
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

