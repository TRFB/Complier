#include"symtable.hpp"
#include"ast.hpp"
#include"ir.hh"
#include <algorithm>
#include <cstddef>
#include<iostream>
#include <vector>
extern FILE* yyin;
extern int yyparse();
extern std::vector<int> arraySubs;
int type;
_ast * astroot;
SymbolTable* symtable=new SymbolTable();
extern IR * IRs;
extern Function * globalFunc;
extern BasicBlock * globalBasic;
extern SymbolTable * symbolTable;
extern past astroot;
int main(int argc, char *argv[]){
    std::map<string,int>mymap={
        {"1",1},{"2",2}
        
    };
    yyin=fopen(argv[1],"r");
	if (!yyin) 
        return 0;
	yyparse();
    IRs->addBlock(globalFunc->getname(),globalBasic);
    symbolTable->addFunc(globalFunc->getname(), globalFunc);
    generateIR(astroot, globalBasic);
    dumpIR(IRs);
    /*
     for (int i : arraySubs) {
        std::cout << i << " ";
    }
    std::cout << std::endl;
    */

	return 0;
}