#include"symtable.hpp"
#include "ast.hpp"
#include "parser.tab.hh"
#include <cstddef>
#include <fstream>
#include <iostream>
#include<string>
#include<map>
#include <algorithm>
#include <vector>
using std::string;

Type Type::getPtrType(){
    Type ptrType=*this;
    /*if(ptrDimen==0){
        ptrType.ptrDimen=1;
        return ptrType;
    }
    else */if(ptrDimen>1){
        ptrType.ptrDimen--;
        return ptrType;
    }
    else if(arraysubs.size()!=0&&ptrDimen==1){
        ptrType.arraysubs.erase(ptrType.arraysubs.begin());
        return ptrType;
    }
    else{
        return ptrType;
    }
    
}

Constant::Constant(Type type,float value){
    this->type=type;
    if(type.type==FLOAT){
        val.fval=value;
    }
    if(type.type==INT){
        val.ival=value;
    }
    if(type.type==I1){
        val.bval=value;
    }
}

sVar::sVar(Variable * firstreg,int num){
    firstReg=firstreg;
    if(firstReg->type.type==FLOAT){
        initVal=new Constant(Type(FLOAT,0),num);
        initVal->sitofp(Type(FLOAT,0));
    }
    else if(firstReg->type.type==INT){
        initVal=new Constant(Type(INT,0),num);
    }
}

sVar::sVar(Variable * firstreg,float num){
    firstReg=firstreg;
    if(firstReg->type.type==INT){
        initVal=new Constant(Type(FLOAT,0),num);
        initVal->fptosi(Type(INT,0));
    }
    else if(firstReg->type.type==FLOAT){
        initVal=new Constant(Type(FLOAT,0),num);
    }
}

bool Variable::isConst(){
    if(type.isConst==true)
        return true;
    else
        return false;
}

bool Function::isParamFind(Variable *var){
    if(std::find(formalParameters.begin(), formalParameters.end(), var)==formalParameters.end())
        return false;
    else
        return true;
}

void Function::addSymbols(Variable * variable){
    symbols[variable->varname()]=variable;
}

bool Variable::isArray(void){
    if(type.arraysubs.size()==0)
        return false;
    else 
        return true;
}

void Scope::addVar(string varName,sVar * tmpReg){
    Variables[varName]=tmpReg;
    symbols[tmpReg->firstReg]=tmpReg;
}

sVar * Scope::lookupThis(string name){
    auto varname=Variables.find(name);
    if(varname!=Variables.end()){
        return Variables[name];
    }
    else {
        return NULL;
    }
}

void Constant::nega(){
    if(type.type==FLOAT)
        val.fval=-val.fval;
    if(type.type==INT) 
        val.ival=-val.ival;
    
}

bool Constant::isZero(){
    if(type.type==FLOAT){
        if(val.fval==0)
            return true;
    }
    if(type.type==INT){
        if(val.ival==0)
            return true;
    }
    if(type.type==I1){
        if(val.bval==0)
            return true;
    }
}

sVar * Scope::lookup(string name){
    Scope * tmp=this;
    sVar * var=tmp->lookupThis(name);
    while(var==NULL){
        tmp=tmp->getpre();
        if(tmp==NULL)
            break;
        var=tmp->lookupThis(name);
    }
    return var;
}

void Function::addParm(Type type,int regNUm){
    Variable * parm=new Variable(type,"%"+std::to_string(regNUm),this);
    formalParameters.push_back(parm);
    return;

}

int Scope::getDimenInitVal(string name){
    sVar * tmp=lookup(name);
    return tmp->initVal->val.ival;
}

bool Scope::isGlobal(){
    if(preScope==NULL)
        return true;
    else
        return false;
}

void SymbolTable::addFunc(string funcName,Function *function){
    symbols[funcName]=function;
}

void Constant::trans(Type tType){
    if(this->type.type==FLOAT&&tType.type==INT){
        fptosi(Type(INT,0));
    }
    else if(this->type.type==INT&&tType.type==FLOAT){
        sitofp(Type(FLOAT,0));
    }
}

void sVar::getInitVals(vector<int> &arraysubs,past astnode,Scope * scope){
    
    if(arraysubs.size()==0){
        vector<int> empty;
        Constant * tmp=getInitValue(astnode->right, scope,empty);
        this->initVal=tmp;
        return ;
    }
    vector<int> tmparraysubs;
      std::reverse_copy(arraysubs.begin(), arraysubs.end(), std::back_inserter(tmparraysubs));
    markArray(astnode->right,tmparraysubs.size());
    for(std::vector<int>::size_type i=0;
             i<tmparraysubs.size();i++)
        editArray(astnode->right,tmparraysubs,astnode->dimen,i);
    setIndex(astnode, 0, arraysubs,scope);
    return ;
}

void sVar::setIndex(past astnode,int index,std::vector<int> & arraysubs,Scope * scope){
    if(astnode==NULL)
        return ;
	if(astnode->left->nodeType!=INIT_LIST_EXPR){
		for(past tmp=astnode->left;tmp!=NULL;tmp=tmp->next){
			initVals[index++]=getInitValue(astnode,scope,arraysubs);
		}
        return ;
	}
    else{
        int i=arraysubs.size()-astnode->dimen-1;
        int tmpindex=1;
        for(;i<=arraysubs.size()-1;i++)
            tmpindex*=arraysubs[i];
        setIndex(astnode->left,index,arraysubs,scope);
        setIndex(astnode->next,index+tmpindex,arraysubs,scope);
    }
}

Constant* getInitValue(past astnode,Scope * scope,std::vector<int> &arraysubs){
    if(astnode->nodeType==INTEGER_LITERAL){
        Constant * val=new Constant(Type(INT,0),astnode->ivalue);
        return val;
    }
    else if(astnode->nodeType==FLOATING_LITERAL){
        Constant * val=new Constant(Type(FLOAT,0),astnode->fvalue);
        return val;
    }
    else if(astnode->nodeType==DECL_REF_EXPR){
        sVar* declVar=scope->lookup(astnode->svalue);
        Constant * val=new Constant(declVar->firstReg->getType(),declVar->initVal);
        return val;
    }
    else if(astnode->nodeType==ARRAY_SUBSCRIPT_EXPR){
        past tmp=astnode;
        int i=0;
        int index=0;
        while(astnode->nodeType==DECL_REF_EXPR){
            if(i==0)
                index+=astnode->left->ivalue;
            else
                index+=arraysubs[arraysubs.size()-i]*(astnode->left->ivalue);
            i++;
            tmp=tmp->right;
        }
        sVar *declVar=scope->lookup(tmp->svalue);
        return declVar->initVals[index];
    }
    else if(astnode->nodeType==BINARY_OPERATOR){
        Constant * leftOpd=getInitValue(astnode->left,scope,arraysubs);
        Constant * rightOpd=getInitValue(astnode->right,scope,arraysubs);
        return calcConstant(leftOpd, rightOpd, astnode->ivalue);
    }
}

Constant* getInitValue2(past astnode,Scope * scope);
Constant* getInitValue2(past astnode,Scope * scope){
    if(astnode->nodeType==INTEGER_LITERAL){
        Constant * val=new Constant(Type(INT,0),astnode->ivalue);
        return val;
    }
    else if(astnode->nodeType==FLOATING_LITERAL){
        Constant * val=new Constant(Type(FLOAT,0),astnode->fvalue);
        return val;
    }
    else if(astnode->nodeType==DECL_REF_EXPR){
        sVar* declVar=scope->lookup(astnode->svalue);
        Constant * val=new Constant(declVar->firstReg->getType(),declVar->initVal);
        return val;
    }
    else if(astnode->nodeType==BINARY_OPERATOR){
        Constant * leftOpd=getInitValue2(astnode->left,scope);
        Constant * rightOpd=getInitValue2(astnode->right,scope);
        return calcConstant(leftOpd, rightOpd, astnode->ivalue);
    }
}

Constant * calcConstant(Constant *leftOpd,Constant* rightOpd,int op){
    Constant *result=new Constant();
    if(leftOpd->getType().type==FLOAT||rightOpd->getType().type==FLOAT){
        result->setType(Type(FLOAT,0));
    }
    switch(op){
            case '+':{
                if(leftOpd->type.type==FLOAT&&rightOpd->type.type==FLOAT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.fval+rightOpd->val.fval);
                    return result;
                }
                else if(leftOpd->type.type==FLOAT&&rightOpd->type.type==INT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.fval+rightOpd->val.ival);
                    return result;
                }
                else if(leftOpd->type.type==INT&&rightOpd->type.type==FLOAT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.ival+rightOpd->val.fval);
                    return result;
                }
                else if(leftOpd->type.type==INT&&rightOpd->type.type==INT){
                    result->setType(Type(INT,0));
                    result->setVal(leftOpd->val.ival+rightOpd->val.ival);
                    return result;
                }
            }
            break;
            case '-':{
                if(leftOpd->type.type==FLOAT&&rightOpd->type.type==FLOAT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.fval-rightOpd->val.fval);
                    return result;
                }
                else if(leftOpd->type.type==FLOAT&&rightOpd->type.type==INT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.fval-rightOpd->val.ival);
                    return result;
                }
                else if(leftOpd->type.type==INT&&rightOpd->type.type==FLOAT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.ival-rightOpd->val.fval);
                    return result;
                }
                else if(leftOpd->type.type==INT&&rightOpd->type.type==INT){
                    result->setType(Type(INT,0));
                    result->setVal(leftOpd->val.ival-rightOpd->val.ival);
                    return result;
                }
            }
            break;
            case '*':{
                if(leftOpd->type.type==FLOAT&&rightOpd->type.type==FLOAT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.fval*rightOpd->val.fval);
                    return result;
                }
                else if(leftOpd->type.type==FLOAT&&rightOpd->type.type==INT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.fval*rightOpd->val.ival);
                    return result;
                }
                else if(leftOpd->type.type==INT&&rightOpd->type.type==FLOAT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.ival*rightOpd->val.fval);
                    return result;
                }
                else if(leftOpd->type.type==INT&&rightOpd->type.type==INT){
                    result->setType(Type(INT,0));
                    result->setVal(leftOpd->val.ival*rightOpd->val.ival);
                    return result;
                }
            }
            break;
            case '/':{
                if(leftOpd->type.type==FLOAT&&rightOpd->type.type==FLOAT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.fval/rightOpd->val.fval);
                    return result;
                }
                else if(leftOpd->type.type==FLOAT&&rightOpd->type.type==INT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.fval/rightOpd->val.ival);
                    return result;
                }
                else if(leftOpd->type.type==INT&&rightOpd->type.type==FLOAT){
                    result->setType(Type(FLOAT,0));
                    result->setVal(leftOpd->val.ival/rightOpd->val.fval);
                    return result;
                }
                else if(leftOpd->type.type==INT&&rightOpd->type.type==INT){
                    result->setType(Type(INT,0));
                    result->setVal(leftOpd->val.ival/rightOpd->val.ival);
                    return result;
                }
            }
                break;
            case '%':{
                if(leftOpd->type.type==INT&&rightOpd->type.type==INT){
                    result->setType(Type(INT,0));
                    result->setVal(leftOpd->val.ival%rightOpd->val.ival);
                    return result;
                }
            }
            break;
            default:break;
        }
}

string Type::TypeToString(){
    string typeStr;
    if(type==INT){
        if(arraysubs.size()==0){
            typeStr+="i32";
        }    
        else{
            for (int dimen : arraysubs) {
                typeStr+=(std::to_string(dimen)+"x"); 
            }
            typeStr+="i32";
        }
    }
    if(type==FLOAT){
        if(arraysubs.size()==0){
            typeStr+="float";
        }    
        else{
            for (int dimen : arraysubs) {
                typeStr+=(std::to_string(dimen)+"x"); 
            }
            typeStr+="float";
        }
    }
    if(type==I1){
        if(arraysubs.size()==0){
            typeStr+="i1";
        }    
        else{
            for (int dimen : arraysubs) {
                typeStr+=(std::to_string(dimen)+"x"); 
            }
            typeStr+="i1";
        }
    }
    if(type==VOID){
        typeStr+="void";
    }
    int tmp=ptrDimen;
    while(tmp!=0){
        typeStr+="*";
        tmp--;
    }
    return typeStr;
    std::cout<<"type print error"<<std::endl;
}

string Constant::SymToString(){
    string symStr;
    symStr+=type.TypeToString();
    symStr+=" ";
    if(type.type==INT)
        symStr+=std::to_string(val.ival);
    if(type.type==FLOAT)
        symStr+=std::to_string(val.fval);
    if(type.type==I1)
        symStr+=std::to_string(val.bval);
    return symStr;
}

string Variable::SymToString(){
    string symStr;
    symStr+=type.TypeToString();
    symStr+=" ";
    symStr+=name;
    return symStr;
}

/*
Variable * symLookUp(string name,Function *function){
    Variable * symbol=function->lookup(name);
    return symbol;
}
*/