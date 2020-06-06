minil: 
	bison -v -d --file-prefix=y mini_l.y
	flex mini_l.lex	
	g++ -o parser y.tab.c lex.yy.c -ll

clean:
	rm -f y.*
	rm -f lex.yy.c
	rm -f parser