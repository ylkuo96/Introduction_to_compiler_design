/**
 * Introduction to Compiler Design by Prof. Yi Ping You
 * Prjoect 2 main function
 */
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

using namespace std;

extern int yyparse();	/* declared by yacc */
extern FILE* yyin;	/* declared by lex */
extern char fileName[256];

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
	// ---
	// parse argv[1] and setup fileName
	const char* slashPos = strrchr(argv[1], '/');
	size_t fileNameLength;
	if( slashPos )
		strcpy(fileName, slashPos+1);
	else
		strcpy(fileName, argv[1]);
	fileNameLength = strlen(fileName);	
	// cout<<"---"<<endl;
	// cout<<fileName<<endl;
	
	if( fileName[fileNameLength-2] != '.' || fileName[fileNameLength-1] != 'p' ) {
		fprintf( stdout, "input file: %s \tshould be [fileName].p \n", fileName );	
		exit(-1);
	}
	fileName[fileNameLength-2] = '\0';
	// ---
	yyin = fp;
	yyparse();	/* primary procedure of parser */
	/*
	fprintf( stdout, "\n|--------------------------------|\n" );
	fprintf( stdout, "|  There is no syntactic error!  |\n" );
	fprintf( stdout, "|--------------------------------|\n" );*/
	exit(0);
}

