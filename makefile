
all: clean parser

parser: parser.l parser.y
	bison -d -o parser.tab.cc parser.y
	flex -o lex.yy.cc parser.l
	g++ -o $@ parser.tab.cc lex.yy.cc ast.hpp ast.cpp symtable.hpp main.cc symtable.cpp ir1.cpp ir.hh tool.cpp tool.h -g

clean:
	rm -rf parser lex.yy.cc parser.tab.cc parser.tab.hh 
