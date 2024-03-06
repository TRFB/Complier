#ifndef IR_H
#define IR_H
#include"ast.hpp"
#include"symtable.hpp"
#include<list>
#include <string>
#include<utility>
using std::list;
using std::pair;
using std::to_string;




class BasicBlock {
public:
    BasicBlock(string name):name(name){}
    void addIR(Instruction * instruction){instructions.push_back(instruction);}
    void addPred(BasicBlock *bblock){Preds.push_back(bblock);}
    void addSucc(BasicBlock *bblcok){Succs.push_back(bblcok);}
    Instruction * lastInstruction(){return instructions.back();}
    list<Instruction *> instructions={};
    string bblockName(){return name;}
private:
    string name;
    vector<BasicBlock*> Preds;
    vector<BasicBlock*> Succs;
    
};

class Instruction {
public:
    virtual ~Instruction() {}
};

class tmpConstant:public Instruction{
public:
    tmpConstant(Type type, int value)
        {constant=new Constant(type,value);}
    tmpConstant(Type type,float value)
       {constant=new Constant(type,value);}
    tmpConstant(Type type,bool value)
        {constant=new Constant(type,value);}
    tmpConstant(Type type,Val val){
        constant=new Constant(type,val);
    }
    Constant * getVal(){return constant;}
    Type getType(){return constant->getType();}
    
private:
    Constant * constant;
};


class Load : public Instruction {
public:
    Load(Variable* tReg, Variable* sReg)
        : tReg(tReg), sReg(sReg) {}
    Variable * getsReg(){return sReg;}
    Variable * gettReg(){return tReg;}
private:
    Variable* tReg;
    Variable* sReg;
};



class BinaryOperation : public Instruction {
public:
    BinaryOperation(int op,SymbolEntry* opd1, SymbolEntry* opd2,Variable * tReg)
        : op(op), opd1(opd1), opd2(opd2), tReg(tReg){this->op=optran(op);}
    BinaryOperation(int op,SymbolEntry* opd1, SymbolEntry* opd2,
                    int cmptype,Variable * tReg)
        : op(op), opd1(opd1), opd2(opd2) ,cmptype(cmptype),tReg(tReg){this->op=optran(op);}
    Variable * gettReg(){return tReg;}
    SymbolEntry *getOpd1(){return opd1;}
    SymbolEntry *getOpd2(){return opd2;}
    string OpToString();
    int optran(int sop);
private:
    int op;
    int cmptype=-1;//浮点或者整数比较
    SymbolEntry* opd1;
    SymbolEntry* opd2;
    Variable * tReg;
};

class Alloca : public Instruction {
public:
    Alloca(Type type, Variable * tReg);
    Variable * gettReg(){return tReg;}
private:
    Type type;
    Variable * tReg;
    int alignNum;
};

class Store:public Instruction{
public:
    Store(Variable * tReg,SymbolEntry * sReg)
    :tReg(tReg),sReg(sReg){}
    Variable * gettReg(){return tReg;}
    SymbolEntry * getsReg(){return sReg;}
private:
    Variable * tReg;
    SymbolEntry * sReg;
};

class Phi :public Instruction{
public:
    Phi(Variable * tReg):tReg(tReg){}
    void addPhiSym(SymbolEntry * var,BasicBlock* bblock);
    Variable * gettReg(){return tReg;}
    map<BasicBlock *,SymbolEntry *> phiTable;
private:
    Variable * tReg;
};

class Branch: public Instruction {
public:
    Branch(SymbolEntry* Reg, BasicBlock* trueBlock,BasicBlock* falseBlock)
        : Reg(Reg),trueBlock(trueBlock),falseBlock(falseBlock) {}
    SymbolEntry * getReg(){return Reg;}
    BasicBlock * gettrueBlock(){return trueBlock;}
    BasicBlock * getfalseBlock(){return falseBlock;}
private:
    SymbolEntry* Reg;
    BasicBlock * trueBlock;
    BasicBlock * falseBlock;
};


class Jump : public Instruction {
public:
    Jump(BasicBlock* BBlock)
        :BBlock(BBlock) {}
    BasicBlock * getBblock(){return BBlock;}
private:
    BasicBlock* BBlock;
};

class Ret :public Instruction{
public:
    Ret(Constant * retVal):retVal(retVal){}
    Ret(Variable * retVal):retVal(retVal){}
    SymbolEntry * getVal(){return retVal;}
private:
    SymbolEntry * retVal;
};

class Global:public Instruction{
public:
    Global(Variable *var,Constant * initVal,map<int,Constant*> initVals)
    :var(var),initVal(initVal),initVals(initVals){}
private:
    Variable * var;
    Constant * initVal;
    map<int,Constant *> initVals;
};

class TypeTran:public Instruction{
public:
    TypeTran(Variable * sReg,
    Variable * tReg,
    Type tType,
    int transType):sReg(sReg),tReg(tReg),tType(tType),transType(transType){}
    Variable * gettReg(){return tReg;}
    Variable * getsReg(){return sReg;}
    string IRToString();
    Type tType;
    int transType;
private:
    Variable * sReg;
    Variable * tReg;
    
    
};

class GetElementPtr:public Instruction{
public:
    GetElementPtr(Variable *sptr,int offset,Variable * tptr)
    :sptr(sptr),offset(offset),tptr(tptr){}
    
    Variable * getTptr(){return tptr;}
    Variable * getSptr(){return sptr;}
    int offset;
private:
    Variable * sptr;
    Variable * tptr;
    
};

class GetPtr:public Instruction{
public:
    GetPtr(Variable *sptr,int offset,Variable * tptr)
    :sptr(sptr),offset(offset),tptr(tptr){}
    
    Variable * getTptr(){return tptr;}
    Variable * getSptr(){return sptr;}
    int offset;
private:
    Variable * sptr;
    Variable * tptr;
    
};

class CallStmt:public Instruction{
public:
    CallStmt(Function * function):function(function){}
    void addParams(SymbolEntry* parameter){params.push_back(parameter);}
    void setRetVal(Variable * var){retVal=var;}
    Function * getFunc(){return function;}
    vector<SymbolEntry *> params;
private:
    Variable * retVal;
    
    Function * function;

};

class IR{
public:
    void addBlock(string funcname,BasicBlock * bblock);
    typedef list< BasicBlock*> BasicBlocks;
    map<string,BasicBlocks> irs;
private:
    
    
};
Instruction * generateIR(past astnode,BasicBlock * curBblock);
void dumpIR(IR * IRs);
#endif