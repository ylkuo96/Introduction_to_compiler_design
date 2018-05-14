%{
#define LIST               strcat(buf, yytext)    
#define token(t)           {LIST; if (Opt_T) printf("<%s>\n", #t);}
#define tokenChar(t)       {LIST; if (Opt_T) printf("<%c>\n", (t));}
#define tokenInteger(t, i) {LIST; if (Opt_T) printf("<%s: %d>\n", #t, (i));}
#define tokenString(t, s)  {LIST; if (Opt_T) printf("<%s: %s>\n", #t, (s));}
#define MAX_LINE_LENG      256

int Opt_S = 1;
int Opt_T = 1;
int linenum = 1;
char buf[MAX_LINE_LENG];

%}

/*---------------------------------------------------------*/
digit [0-9]
letter [a-zA-Z]
/*real ([0-9]*)|([0-9]+\.[0-9]*)*/
/*---------------------------------------------------------*/

%START COMMENT
%%
 /*Tabs & Whitespace*/
<INITIAL>\t	LIST;
<INITIAL>" "	LIST;
 /*Comments*/
<COMMENT>.	LIST;
<COMMENT>\n	{
	LIST; // put \n in buf
	if (Opt_S)
	  printf("%d: %s", linenum, buf);
	linenum++;
	buf[0] = '\0'; }
<COMMENT>"*/"	{ LIST; BEGIN INITIAL; }
<INITIAL>"/*".*	{ LIST; BEGIN COMMENT; }
 /*Pseudocomments*/
<INITIAL>"//&S+".* { LIST; Opt_S=1; }
<INITIAL>"//&S-".* { LIST; Opt_S=0; }
<INITIAL>"//&T+".* { LIST; Opt_T=1; }
<INITIAL>"//&T-".* { LIST; Opt_T=0; }
 /*Comments*/
<INITIAL>"//".*	LIST;
 /*Newline*/
\n 	{
	LIST; // put \n in buf
	if (Opt_S)
	  printf("%d: %s", linenum, buf);
	linenum++;
	buf[0] = '\0'; }
 /*Delimeters*/
<INITIAL>"("     	{ tokenChar('('); }
<INITIAL>","		{ tokenChar(','); }
<INITIAL>";"		{ tokenChar(';'); }
<INITIAL>":"		{ tokenChar(':'); }
<INITIAL>")"		{ tokenChar(')'); }
<INITIAL>"["		{ tokenChar('['); }
<INITIAL>"]"		{ tokenChar(']'); }
 /*Operator*/
<INITIAL>"+"		{ tokenChar('+'); }
<INITIAL>"-"		{ tokenChar('-'); }
<INITIAL>"*"		{ tokenChar('*'); }
<INITIAL>"/"		{ tokenChar('/'); }
<INITIAL>"mod"		{ token(mod); }
<INITIAL>":="		{ token(:=); }
<INITIAL>"<"		{ tokenChar('<'); }
<INITIAL>"<="		{ token(<=); }
<INITIAL>"<>"		{ token(<>); }
<INITIAL>">="		{ token(>=); }
<INITIAL>">" 		{ tokenChar('>'); }
<INITIAL>"="		{ tokenChar('='); }
<INITIAL>"and"		{ token(and); }
<INITIAL>"or"		{ token(or); }
<INITIAL>"not"		{ token(not); }
 /*Keywords*/
<INITIAL>array		{ token(KWarray); }
<INITIAL>begin		{ token(KWbegin); }
<INITIAL>boolean	{ token(KWboolean); }
<INITIAL>def		{ token(KWdef); }
<INITIAL>do		{ token(KWdo); }
<INITIAL>else		{ token(KWelse); }
<INITIAL>end		{ token(KWend); }
<INITIAL>false		{ token(KWfalse); }
<INITIAL>for		{ token(KWfor); }
<INITIAL>integer	{ token(KWinteger); }
<INITIAL>if		{ token(KWif); }
<INITIAL>of       	{ token(KWof); }            
<INITIAL>print     	{ token(KWprint); }            
<INITIAL>read      	{ token(KWread); }               
<INITIAL>real      	{ token(KWreal); }                
<INITIAL>string    	{ token(KWstring); }            
<INITIAL>then      	{ token(KWthen); }               
<INITIAL>to        	{ token(KWto); }           
<INITIAL>true  		{ token(KWtrue); }
<INITIAL>return    	{ token(KWreturn); }
<INITIAL>var       	{ token(KWvar); } 
<INITIAL>while     	{ token(KWwhile); }
 /*Integer Constants*/
<INITIAL>0|([1-9][0-9]*) { tokenInteger(integer, atoi(yytext)); } 
 /*Integer with Oct*/
<INITIAL>0[0-7]+ { tokenString(oct_integer,yytext); }
 /*Identifiers*/
<INITIAL>{letter}({letter}|{digit})* { tokenString(id,yytext); }
 /*Floating-Point Constants*/
<INITIAL>[0-9]+\.[0-9]+ { tokenString(float,yytext); /* 01.234 */}
 /*Scientiﬁc Notations*/
<INITIAL>(([0-9]+)|([0-9]+\.[0-9]+))[Ee][+-]?([0-9]+) { tokenString(scientific,yytext); /* 012E+4 */}
 /*String Constants*/
<INITIAL>\"([^"\n]|\"{2})*\" { 
	if(yyleng==2){ // deal with the case ""
		char result[yyleng-1];
		result[0]='\0';
		char *p = &result[0];
		tokenString(string,p);
	}
	else{
		int i=1, j=0; // i=1 because "
		char result[yyleng];
		while(i<yyleng){
		  if(yytext[i]=='\"' && yytext[i+1]=='\"'){
			i++; result[j++]=yytext[i++];
		  }
		  else{
		  	result[j++]=yytext[i++];
		  }
		}
		result[j-1]='\0'; // the end '\"'
		char *p = &result[0];
		tokenString(string,p);
	}
 } 

 /*-------------------------ERROR---------------------------*/
. { printf("error at line %d, bad character: %s\n", linenum, yytext); exit(0); }
 /*-------------------------ERROR---------------------------*/
%%

int main( int argc, char **argv )
{
  if( argc != 2 ) {
    fprintf( stderr, "Usage: ./scanner [filename]\n" );
    exit(0);
  }
	
  FILE *fp = fopen( argv[1], "r" );
  if( fp == NULL ) {
    fprintf( stderr, "Open file error\n" );
    exit(-1);
  }
	
  yyin = fp;

  yylex();

  exit(0);
}
