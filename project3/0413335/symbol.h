#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <vector>
#include <string>
#include <sstream>

extern char *yytext;
extern int linenum;
extern int Opt_D;

using namespace std;

struct entry{
	string name; // at most length 32
	string kind; // program, function, parameter, variable, and constant
	int level; // 0 is global, 1~ is local
	
	string type; // integer, real, boolean, string (may have array type)
	vector<int> type_dim; // 0 if it is not array
	
	vector<string> attr; // constants, or function paramater (may have array type)
	vector<vector<int> > attr_dim; // 0 if it is not array 
};

struct symbolTable{
	vector<entry> tableEntry;
	int level;
};

void dumpsymbol(symbolTable T) {
	if(Opt_D){
		//int i; 
		for(int i=0;i< 110;i++) 
			printf("="); 
		printf("\n"); 
		printf("%-33s%-11s%-11s%-17s%-11s\n","Name","Kind","Level","Type","Attribute"); 
		for(int i=0;i< 110;i++) 
			printf("-"); 
		printf("\n"); 
		for(int i=0;i<T.tableEntry.size();i++)
		{ 
			printf("%-33s", T.tableEntry[i].name.c_str()); // name
			printf("%-11s", T.tableEntry[i].kind.c_str()); // kind
			if(T.tableEntry[i].level==0){
				printf("%d%-10s", T.tableEntry[i].level, "(global)");  // level
			}
			else{
				printf("%d%-10s", T.tableEntry[i].level, "(local)");  // level
			}
			
			if(T.tableEntry[i].type_dim.size()>0){
				string QQ=T.tableEntry[i].type;
				for(int j=0;j<T.tableEntry[i].type_dim.size();j++){
					string QAQ;
					stringstream s;
					s<<T.tableEntry[i].type_dim[j];
					s>>QAQ;
					QQ+=" ["+QAQ+"]";
				}
				printf("%-17s", QQ.c_str());
			}
			else{
				printf("%-17s", T.tableEntry[i].type.c_str());
			}

			if(!T.tableEntry[i].attr.empty()){
				for(int j=T.tableEntry[i].attr.size()-1;j>=0;j--){
					if(T.tableEntry[i].kind == "function" || T.tableEntry[i].kind == "parameter"){
						string Q_Q=T.tableEntry[i].attr[j];
						if(T.tableEntry[i].attr_dim[j].size()>0){
							vector<int> QWQ;
							QWQ.assign(T.tableEntry[i].attr_dim[j].begin(), T.tableEntry[i].attr_dim[j].end());
							for(int k=0;k<QWQ.size();k++){
								string QAQ;
								stringstream s;
								s<<QWQ[k];
								s>>QAQ;
								Q_Q+=" ["+QAQ+"]";
							}
						}
						 
						if(j==0){printf("%s", Q_Q.c_str()); }
						else{
							printf("%s, ", Q_Q.c_str());
						}
					}
					if (T.tableEntry[i].kind == "constant") {
						//printf("%-11s,", T.tableEntry[i].attr[j].c_str());
						if(j==0){printf("%s", T.tableEntry[i].attr[j].c_str()); }
						else{
							printf("%s, ", T.tableEntry[i].attr[j].c_str());
						}
					}
				} 
			}
			printf("\n"); 
		}
		for(int i=0;i< 110;i++)
			printf("-"); 
		printf("\n");
	}
}