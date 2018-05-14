%{
#include <stdio.h>
#include <stdlib.h>
extern int linenum;             /* declared in lex.l */
extern FILE *yyin;              /* declared by lex */
extern char *yytext;            /* declared by lex */
extern char buf[256];           /* declared in lex.l */
%}

%token IDENT MOD ASSIGN LESSTHAN NOEQUAL GREATERTHAN AND OR NOT
%token ARRAY KWBEGIN TYPE DEF DO ELSE END FALSE FOR IF OF PRINT
%token READ THEN TO TRUE RETURN VAR WHILE OCT INT FLOAT SCI STRING

%left OR
%left AND
%left NOT
%left  '<' LESSTHAN '=' '>' GREATERTHAN NOEQUAL
%left '+' '-'
%left '*' '/' MOD
%left '(' ')'

%%

program		: programname ';' programbody END IDENT
			;

programname	: identifier
			;

identifier	: IDENT
			;

programbody : variableSet functionSet compound
			;
	
variableSet : epsilon
			| nonemptyVariableSet
			;

nonemptyVariableSet : nonemptyVariableSet variable
					| variable
					;

variable : VAR idList ':' TYPE ';'
		 | VAR idList ':' ARRAY integerConst TO integerConst OF ARRTYPE ';'
		 | VAR idList ':' literalConstant ';'
		 ;

ARRTYPE : ARRAY integerConst TO integerConst OF ARRTYPE
		| TYPE
		;
		 
integerConst : INT
			 | OCT
			 ;
			 
literalConstant : INT
				| OCT
				| FLOAT 
				| SCI
				| STRING
				| TRUE
				| FALSE
				;
	
functionSet : epsilon
			| nonemptyFunctionSet
			;
			
nonemptyFunctionSet : nonemptyFunctionSet function
					| function
					;

function	: identifier '(' arguments ')' ':' ARRTYPE ';' compound END identifier
			| identifier '(' arguments ')' ';'  compound END identifier // no return value
			;

arguments	: epsilon
			| nonemptyArguments
			;

nonemptyArguments : idList ':' ARRTYPE 
				  | idList ':' ARRTYPE ';' nonemptyArguments
				  ;

idList	: identifier // no way to have an empty idlist
	 	| nonemptyIdList identifier
	 	;

nonemptyIdList	: identifier ','
				| nonemptyIdList identifier ','
				;

epsilon :
		;
		
/*statements*/

statements : epsilon
		   | nonemptyStatements
		   ;

nonemptyStatements : nonemptyStatements statement
				   | statement
				   ;

statement : compound 
          | simple 
          | conditional
          | while
          | for
          | return
    	  | functionInvocation
		  ;

compound : KWBEGIN variableSet statements END 
		 ;	
		 
simple : variableReference ASSIGN expression ';'
	   | PRINT variableReference ';'
	   | PRINT expression ';'
       | READ variableReference ';'
	   ;

variableReference : identifier	   
				  | ArrayReference
				  ;
				  
ArrayReference : identifier '[' integerExpression ']' ArrayReference_
			   ;
			   
ArrayReference_ : epsilon
				| '[' integerExpression ']' ArrayReference_
			    ;

expression : arithmeticExpression
		   | booleanExpression
		   ;

arithmeticExpression : '(' arithmeticExpression ')'
		   | '-' arithmeticExpression %prec '*'
		   | arithmeticExpression '+' arithmeticExpression
		   | arithmeticExpression '-' arithmeticExpression
		   | arithmeticExpression '*' arithmeticExpression
		   | arithmeticExpression '/' arithmeticExpression
		   | arithmeticExpression MOD arithmeticExpression
		   | literalConstant // INT OCTAL SCI FLOAT TRUE FALSE STRING
		   | variableReference // arrayReference identifier
		   | identifier '(' expressionSet ')' // note that it's functionInvocation with no semicolon!
		   ;
		   
booleanExpression : arithmeticExpression '>' arithmeticExpression
				  | arithmeticExpression '<' arithmeticExpression
				  | arithmeticExpression GREATERTHAN arithmeticExpression
				  | arithmeticExpression LESSTHAN arithmeticExpression
				  | arithmeticExpression '=' arithmeticExpression
				  | arithmeticExpression NOEQUAL arithmeticExpression
				  | NOT expression
				  | expression AND expression
				  | expression OR expression
				  ;

integerExpression : expression
				  ;

conditional : IF booleanExpression THEN statements END IF
			| IF booleanExpression THEN statements ELSE statements END IF
			;

while : WHILE booleanExpression DO statements END DO
	  ;

for : FOR identifier ASSIGN integerConst TO integerConst DO statements END DO
	;

return : RETURN expression ';'
	   ;

functionInvocation : identifier '(' expressionSet ')' ';'
				   ;

expressionSet : epsilon
			  | nonemptyExpressionSet
			  ;

nonemptyExpressionSet : expression
					  | nonemptyExpressionSet ',' expression
					  ;
					  
%%

int yyerror( char *msg )
{
        fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
		fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, buf );
		fprintf( stderr, "|\n" );
		fprintf( stderr, "| Unmatched token: %s\n", yytext );
        fprintf( stderr, "|--------------------------------------------------------------------------\n" );
        exit(-1);
}

int  main( int argc, char **argv )
{
	if( argc != 2 ) {
		fprintf(  stdout,  "Usage:  ./parser  [filename]\n"  );
		exit(0);
	}

	FILE *fp = fopen( argv[1], "r" );
	
	if( fp == NULL )  {
		fprintf( stdout, "Open  file  error\n" );
		exit(-1);
	}
	
	yyin = fp;
	yyparse();

	fprintf( stdout, "\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	fprintf( stdout, "|  There is no syntactic error!  |\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	exit(0);
}

