#ifndef SYMBOL_H
#define SYMBOL_H
#include <algorithm>
#include <cstddef>
#include <initializer_list>
#include<string>
#include<list>
#include<vector>
#include<map>
#include<fstream>
#include"ast.hpp"
#include"parser.tab.hh"


class Instruction;
using std::vector;using std::string;using std::list;
using std::map;

union Val{
        int ival;
        float fval;
        bool bval;
    };

class Type{
public:
    Type(){}
    Type(int type,int ptrDimen):type(type),ptrDimen(ptrDimen){
        if(type==const_float){this->type=FLOAT;isConst=true;}
        if(type==const_int){this->type=INT;isConst=true;}
    }
    Type(int type,int ptrDimen,bool constMark)
        :type(type),ptrDimen(ptrDimen),isConst(constMark){
            if(type==const_float){this->type=FLOAT;isConst=true;}
            if(type==const_int){this->type=INT;isConst=true;}
        }
    Type(int type,vector<int> arraysubs,int ptrDimen)
        :type(type),arraysubs(arraysubs),ptrDimen(ptrDimen){
        if(type==const_float){this->type=FLOAT;isConst=true;}
        if(type==const_int){this->type=INT;isConst=true;}
    }
    Type(int type,vector<int> arraysubs,int ptrDimen,bool constMark)
        :type(type),arraysubs(arraysubs),ptrDimen(ptrDimen),isConst(constMark){
            if(type==const_float){this->type=FLOAT;isConst=true;}
            if(type==const_int){this->type=INT;isConst=true;}
        }
    string TypeToString();
    Type getPtrType();
    int type;
    vector<int> arraysubs;
    int ptrDimen=0;
    bool isConst=false;
};


class SymbolEntry {
public:
    virtual ~SymbolEntry() {}
    virtual string SymToString(){return "";};
};

class Variable;

class Function : public SymbolEntry {
public:
    Function(std::string name, Type returnType)
        : name(name), returnType(returnType) {}
    void addParm(Type type){formalParameters.push_back(type);return;}
    void addSymbols(Variable * variable);
    Variable * lookup(string name);
    string getname(){return name;}
    std::vector<Type> formalParameters;
    Type getRetType(){return returnType;}
private:
    std::string name;
    Type returnType;
    std::map<string,Variable *> symbols;
    //指令
};


class Constant : public SymbolEntry {
public:
    Constant(){};
    Constant(Type type,float value);
    Constant(Type type, Val value)
        : type(type), val(value) {}
    Constant(Type type, int value)
        : type(type), val({.ival=value}) {}
    Constant(Type type,bool value)
        :type(type),val({.bval=value}){}
    Type getType(){return type;}
    void nega();
    void setType(Type settype){type=settype;}
    void setVal(int value){val.ival=value;}
    void setVal(float value){val.fval=value;}
    void trans(Type tType);
    void fptosi(Type newType){type=newType;val.ival=val.fval;}
    void sitofp(Type newType){type=newType;val.fval=val.ival;}
    void botofp(Type newType){type=newType;val.fval=val.bval;}
    void sitobo(Type newType){type=newType;val.bval=val.ival;}
    void fptobo(Type newType){type=newType;val.bval=val.fval;}
    void botosi(Type newType){type=newType;val.ival=val.bval;}
    bool isZero();
    string SymToString() override;
    Val val;
    friend Constant * calcConstant(Constant *leftOpd,Constant* rightOpd,int op);
    // 重写虚函数
     
private:
    Type type;
};

class Variable : public SymbolEntry {
public:
    //Variable(Type type,string name,Function *function,vector<int> arraysubs)
     //   : type(type), name(name),function(function) {}
    Variable(Type type,string name,Function *function)
        : type(type), name(name),function(function){}
    string varname(){return name;}
    Type  getType(){return type;}
    bool isArray(void);
    bool isConst();
    string SymToString() override;
    Type type;
    //std::vector<int> arraysubs={};//元素个数为0时表示数组
private:
    
    string name; 
    Function * function=NULL;
    
    //后续扩展真实寄存器
};
class Scope;

class sVar:public SymbolEntry{
public:
    sVar(Variable * firstReg)
    :firstReg(firstReg){};
    sVar(Variable * firstReg,Constant * initVal)
    :firstReg(firstReg),initVal(initVal){};
    sVar(Variable * firstreg,int num);
    sVar(Variable * firstreg,float num);
    void getInitVals(vector<int> &arraysubs,past astnode,Scope * scope);
    void setIndex(past astnode,int index,std::vector<int> & arraysubs,Scope * scope);
    Variable * firstReg;
    Constant * initVal=NULL;
    map<int, Constant*> initVals;
};

class Scope :public SymbolEntry{
public:
    Scope()=default;
    void setPreScope(Scope * scope){preScope=scope; (*preScope).nextScopes.push_back(scope);}
    void addVar(string varName,sVar * tmpReg);
    bool isGlobal();
    sVar * lookupThis(string name);
    sVar* lookup(string name);
    sVar* lookup(Variable * var){return symbols[var];}
    int getDimenInitVal(string name);
    int getThisDimenInitVal(string name){return Variables[name]->initVal->val.ival;}
    Scope * getpre(){return preScope;}
    vector<Scope *> nextScopes;
private:
    //虚拟寄存器编号查询
    std::map<Variable *,sVar *> symbols;
    map<string,sVar *> Variables;//该作用域中声明的变量及其对应的第一个虚拟寄存器
    Scope * preScope=NULL;//该作用域的上一层作用域，为空时代表Global
    
};



class Pointer : public SymbolEntry {
public:
    Pointer(std::string name)
        :name(name) {}
private:
    std::string name;
    vector<int> arraysub;
    Function * scope=NULL;
};

class SymbolTable{
public:
    SymbolTable()=default;
    void addFunc(string funcName,Function *);
    Function * lookupFunc(string name){return symbols[name];}
private:
    map<string,Function *> symbols;

};

Constant* getInitValue(past astnode,Scope * scope,std::vector<int> &arraysubs);
Constant * calcConstant(Constant *leftOpd,Constant* rightOpd,int op);
#endif