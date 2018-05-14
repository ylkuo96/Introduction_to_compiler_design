scanner.out:lex.yy.c
	gcc -o scanner lex.yy.c -lfl
lex.yy.c:lex.l
	lex lex.l
clean:
	rm -f lex.yy.c
