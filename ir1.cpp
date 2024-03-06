#include"ast.hpp"
#include <cstddef>
#include <cstdlib>
#include <exception>
#include <functional>
#include <ostream>
#include <string>
#include<iostream>
#include<vector>
#include<list>
#include <algorithm> // 包含std::find函数
#include<fstream>
#include <utility>
#include "parser.tab.hh"
#include"symtable.hpp"
#include"ir.hh"
using std::cout;
using std::vector;
using std::endl;
using std::pair;
using std::make_pair;
//while语法树修剪优化
//前驱和后继的设置
//Global改为二元运算
//const变量的替代
//局部数组的初始化
//call返回值的修改
//函数形参的数组

//测试const变量表达式的初始化

//括号表达式和=的实现

//局部数组初始化的类型转换

//将局部数组引入二元运算、用多维数组的引用初始化其余变量

//比较运算符的实现，while if的调试、一元运算符的实现
//在while中测试比较运算符
//在while中测试更加复杂的条件表达式、数组在条件表达式中的出现
//while与if嵌套的调试
//if的短路问题

Function * globalFunc=new Function(string("0GLOBAL"),Type(VOID,0));
Function * curFunc=globalFunc;
Scope * globalScope=new Scope();
Scope * scope = globalScope;
int regNum=0;
int bblockNum=0;
vector<BasicBlock*> continueBblock;
vector<BasicBlock*>breakBblock;
IR* IRs=new IR();
SymbolTable * symbolTable=new SymbolTable();
BasicBlock * globalBasic=new BasicBlock("."+to_string(bblockNum++));
bool initListing=false;
vector<past> condDeclRef;

void IR::addBlock(string funcname,BasicBlock * bblock){
    auto it = std::find(irs[funcname].begin(), irs[funcname].end(), bblock);
    if(it == irs[funcname].end())
        irs[funcname].push_back(bblock);
    else
        return ;
    return ;
}

Alloca::Alloca(Type type,Variable * tReg)
    : type(type), tReg(tReg){
    if((tReg->getType()).arraysubs.size()!=0){
        alignNum=(tReg->getType()).arraysubs[0]*4;
    }
    else {
        alignNum=4;
    }
}

void Phi::addPhiSym(SymbolEntry * var,BasicBlock* bblock){
    phiTable[bblock]=var;
}

string TypeTran::IRToString(){
    if(transType==SITOFP)
        return "sitofp";
    if(transType==FPTOSI)
        return "fptosi";
    if(transType==BITCAST)
        return "bitcast";
    if(transType==TRUNC)
        return "trunc";
    if(transType==ZEXT)
        return "zext";
}

string BinaryOperation::OpToString(){
    string opStr;
    if(cmptype==ICMP) opStr+="icmp";
    if(cmptype==FCMP) opStr+="fcmp";
    if(op==ADD) opStr+="add";
    if(op==SUB) opStr+="sub";
    if(op==MUL) opStr+="mul";
    if(op==DIV) opStr+="div";
    if(op==MOD) opStr+="mod";
    if(op==XOR) opStr+="xor";
    if(op==CMP) opStr+="cmp";
    if(op==ICMP) opStr+="icmp";
    if(op==FCMP) opStr+="fcmp";
    if(op==SLT) opStr+=" slt";
    if(op==SLE) opStr+=" sle";
    if(op==SGT) opStr+=" sgt";
    if(op==SGE) opStr+=" sge";
    if(op==SNE) opStr+=" sne";
    if(op==SEQ) opStr+=" seq";
    if(op==OLT) opStr+=" olt";
    if(op==OLE) opStr+=" ole";
    if(op==OGT) opStr+=" ogt";
    if(op==OGE) opStr+=" oge";
    if(op==ONE) opStr+=" one";
    if(op==OEQ) opStr+=" oeq";
    return opStr;
}

int BinaryOperation::optran(int sop){
    if(sop=='+') return ADD;
    if(sop=='-') return SUB;
    if(sop=='*') return MUL;
    if(sop=='/') return DIV;
    if(sop=='%') return MOD;
    if(sop=='<'&&cmptype==ICMP) return SLT;
    if(sop=='<'&&cmptype==FCMP) return OLT;
    if(sop=='>'&&cmptype==ICMP) return SGT;
    if(sop=='>'&&cmptype==FCMP) return OGT;
    if(sop==LES_EQUAL&&cmptype==ICMP) return SLE;
    if(sop==LES_EQUAL&&cmptype==FCMP) return OLE;
    if(sop==GRT_EQUAL&&cmptype==ICMP) return SGE;
    if(sop==GRT_EQUAL&&cmptype==FCMP) return OGE;
    if(sop==EQUAL&&cmptype==ICMP) return SEQ;
    if(sop==EQUAL&&cmptype==FCMP) return OEQ;
    if(sop==NOT_EQUAL&&cmptype==ICMP) return SNE;
    if(sop==NOT_EQUAL&&cmptype==FCMP) return ONE;
}

BasicBlock * get_WhileCond(vector<pair<past,BasicBlock *>> &whileCond,past astnode,BasicBlock *preBblock);
BasicBlock * get_jumpBblock(vector<pair<past,BasicBlock *>> &whileCond,vector<pair<past, BasicBlock *>>::iterator it,BasicBlock* resultBblock);
void get_ifConds_Bodys(past astnode,vector<vector<pair<past,BasicBlock *>>> &ifConds,vector<pair<past,BasicBlock *>> &ifBodys);
void generateIR_IfCond(vector<vector<pair<past,BasicBlock *>>> &ifConds,
                        vector<vector<pair<past,BasicBlock *>>>::iterator ifCond,
                        vector<pair<past,BasicBlock *>> &ifBodys,
                        BasicBlock *nextBblock);
BasicBlock * get_if_And_falseBblock(vector<vector<pair<past,BasicBlock *>>> &ifConds,
                                    vector<vector<pair<past,BasicBlock *>>>::iterator ifCond,
                                    vector<pair<past,BasicBlock *>>::iterator andCond,
                                    BasicBlock * nextBblock);
void generateIR_IfConds(vector<vector<pair<past,BasicBlock *>>> &ifConds,
                        vector<pair<past,BasicBlock *>> &ifBodys,
                        BasicBlock *nextBblock);
void generateIR_IfBodys(vector<pair<past,BasicBlock*>> &ifBodys,BasicBlock* nextBblock);
int getCmpOp(int op,Type leftType,Type rightType);
vector<int> getDimen(past astnode);
int calcDimen(past astnode);
void  generateIR_InitList(past astnode,BasicBlock * curBblock,int index,Variable * array);
Instruction * generateIR_BinaryOper(past astnode,BasicBlock *curBblock);
SymbolEntry * getParam(past astnode,BasicBlock * curBblock,int index,Function * func);
Instruction * generateIR_Param_Array(past astnode,BasicBlock * curBblock);
past editWhileCond(past astnode);
bool isTrue(past astnode);
bool isFalse(past astnode);
Instruction * generateIR_Tran(BasicBlock* bblock,Instruction * sir,Type ttype);
bool astnodeIsConst(past astnode,Scope* scope);
int getTypeAST(past astnode,Scope * scope);
past calcConstAst(past astnode,Scope * scope);
past editBinaryOper(past astnode,Scope* scope);
SymbolEntry * getOpd(Instruction * sir);
Type getOpdType(Instruction * sir);
Variable * generateIR_Lval(past astnode,BasicBlock * curBblock);
pair<int,sVar *> getArrayIndex(past astnode,int antiDimen);
bool arrayIsConst(past astnode);
void freeAst_remainNext(past astnode);
void shortCircuitDelete(vector<vector<pair<past,BasicBlock *>>> &ifConds,
            vector<pair<past,BasicBlock *>> &ifBodys,BasicBlock * curBblock);
void setFirstCondBblock(vector<vector<pair<past,BasicBlock *>>> &ifConds,
                        vector<pair<past,BasicBlock *>> &ifBodys,BasicBlock *curBblock);
past shortCircuitDeleteAst(past astnode);

Instruction * generateIR(past astnode,BasicBlock * curBblock){
    
    
    if(astnode==NULL){
        return NULL;
    }
    switch(astnode->nodeType){
        case TRANSLATION_UNIT:{
            
            IRs->addBlock(globalFunc->getname(), globalBasic);
            generateIR(astnode->left,globalBasic);

            return NULL;
            }
            break;
        case FUNCTION_DECL:{
            //建立新的基本块
            BasicBlock * bblock=new BasicBlock("."+to_string(bblockNum++));
            curBblock=bblock;
            //设置作用域与形参，形参稍后加入符号表
            regNum=0;
            Function * function= new Function(astnode->svalue,Type(astnode->type,0));
            curFunc=function;
            symbolTable->addFunc(curFunc->getname(),curFunc);
            IRs->addBlock(curFunc->getname(), curBblock);
            
            past tmp=astnode->left;
            while(tmp!=NULL){
                (*function).addParm(Type(tmp->type,tmp->arraysubs,0));
                tmp=tmp->next;
            }
            //生成IR
            //形参赋值
            int parmNum=regNum+1;
            tmp=astnode->left;
            while(tmp!=NULL){
                string sRegName="%"+std::to_string(parmNum+regNum);
                regNum++;
                Variable * sReg=new Variable(Type(tmp->type,tmp->arraysubs,1),sRegName,curFunc);
                Alloca * ir=new Alloca(
                    Type(tmp->type,tmp->arraysubs,1),sReg);
                    sVar * tmpsVar=new sVar(sReg);
                //store语句稍后补上
                    scope->addVar(astnode->svalue,tmpsVar);
                curBblock->addIR(ir);
            }
            
            generateIR(astnode->right,curBblock);
            if(astnode->next==NULL||astnode->next->nodeType!=FUNCTION_DECL){
                curFunc=globalFunc;
            }
            generateIR(astnode->next,globalBasic);
            regNum=0;
            bblockNum=0;
                return NULL;
            }
            
            break;
        case COMPOUND_STMT:{
            //新建作用域
            Scope * tmpScope=new Scope();
            tmpScope->setPreScope(scope);
            scope=tmpScope;
            generateIR(astnode->left,curBblock);
            generateIR(astnode->next,curBblock);
            //退出时重置作用域
            scope=scope->getpre();
            return NULL;
        }
            break;
        case WHILE_STMT:{//记得对body基本块自我分裂
            astnode->left=editWhileCond(astnode->left);
           if(astnode->left->nodeType==BINARY_OPERATOR
           ||astnode->left->nodeType==DECL_REF_EXPR){
                if(astnode->left->ivalue==AND||astnode->left->ivalue==OR){
                    BasicBlock * bodyBblock=new BasicBlock("."+to_string(bblockNum++));
                    BasicBlock * resultBblock=new BasicBlock("."+to_string(bblockNum++));
                    BasicBlock * nextBblock=new BasicBlock("."+to_string(bblockNum++));
                    Variable * sReg=new Variable(Type(I1,0),"%"+to_string(regNum++),curFunc);
                    Phi* conditionPhi=new Phi(sReg);
                    resultBblock->addIR(conditionPhi);

                    vector<pair<past,BasicBlock *>> whileCond;
                    //注意区分作为独立条件的declrefexpr和作为比较运算符操作数的declrefexpr
                    get_WhileCond(whileCond,astnode->left,NULL);
                    
                    continueBblock.push_back((*whileCond.begin()).second);
                    //breakBblock.push_back(nextBblock);

                    Jump * ir_jumpCond=new Jump((*whileCond.begin()).second);
                    curBblock->addIR(ir_jumpCond);
                    for (auto it = whileCond.begin(); it != whileCond.end(); ++it) {
                        switch((*it).first->ivalue){
                            case AND:{
                                BasicBlock * jumpBblock=get_jumpBblock(whileCond,it,resultBblock);
                                Instruction * tmp=(*it).second->lastInstruction();
                                BinaryOperation * cmp=dynamic_cast<BinaryOperation *>(tmp);
                                vector<pair<past, BasicBlock *>>::iterator next=it+1;
                                BasicBlock * nextCond=(*next).second;
                                Branch * ir_branch=new Branch(cmp->gettReg(),nextCond,jumpBblock);
                                (*it).second->addIR(ir_branch);
                                if(jumpBblock==resultBblock){
                                    Constant * var=new Constant(Type(I1,0),false);
                                    conditionPhi->addPhiSym(var, (*it).second);
                                }
                                break;
                            }
                            case OR:{
                                Instruction * tmp=(*it).second->lastInstruction();
                                BinaryOperation * cmp=dynamic_cast<BinaryOperation *>(tmp);
                                vector<pair<past, BasicBlock *>>::iterator next=it+1;
                                BasicBlock * nextCond=(*next).second;
                                Branch * ir_branch=new Branch(cmp->gettReg(),resultBblock,nextCond);
                                (*it).second->addIR(ir_branch);
                                Constant * var=new Constant(Type(I1,0),true);
                                conditionPhi->addPhiSym(var, (*it).second);
                                break;
                            }    
                            default:
                                generateIR((*it).first,(*it).second);
                                IRs->addBlock(curFunc->getname(), (*it).second);
                                break;
                        }
                    }
                    BasicBlock * lastBblock=(*(whileCond.end()-1)).second;
                    conditionPhi->addPhiSym(getOpd(lastBblock->lastInstruction()), lastBblock);
                    Jump* ir_jumpResult=new Jump(resultBblock);
                    lastBblock->addIR(ir_jumpResult);

                    IRs->addBlock(curFunc->getname(), resultBblock);
                    IRs->addBlock(curFunc->getname(), bodyBblock);
                    Branch * ir_branchInResult=new Branch(conditionPhi->gettReg(),bodyBblock,nextBblock);
                    resultBblock->addIR(ir_branchInResult);

                    if(astnode->right->nodeType==COMPOUND_STMT)
                        generateIR(astnode->right, bodyBblock);
                    else{
                        Scope * tmpScope=new Scope();
                        tmpScope->setPreScope(scope);
                        scope=tmpScope;
                        generateIR(astnode->right, bodyBblock);
                        scope=scope->getpre();
                    }
                    IRs->addBlock(curFunc->getname(), nextBblock);
                    if(Jump * ir_last=dynamic_cast<Jump *>(bodyBblock->lastInstruction())){
                        if(ir_last->getBblock()==whileCond.begin()->second){
                            ;
                        }
                        else{
                            bodyBblock->addIR(ir_jumpCond);
                        }
                    }
                    else{
                        bodyBblock->addIR(ir_jumpCond);
                    }
                    continueBblock.pop_back();
                    generateIR(astnode->next,nextBblock);
                }
                else{ 
                    BasicBlock * condBblock=new BasicBlock("."+to_string(bblockNum++));
                    IRs->addBlock(curFunc->getname(), condBblock);
                    BasicBlock * bodyBblock=new BasicBlock("."+to_string(bblockNum++));
                    IRs->addBlock(curFunc->getname(), bodyBblock);
                    BasicBlock * nextBblock=new BasicBlock("."+to_string(bblockNum++));

                    continueBblock.push_back(condBblock);
                    //breakBblock.push_back(nextBblock);

                    Jump* ir_jumpCond=new Jump(condBblock);
                    curBblock->addIR(ir_jumpCond);
                    if(astnode->left->nodeType==DECL_REF_EXPR)
                        condDeclRef.push_back(astnode->left);
                    Instruction * tmp=generateIR(astnode->left,condBblock);
                    //返回的是tmpConstant，应该先优化while cond再开始生成
                    BinaryOperation * cmp=dynamic_cast<BinaryOperation *>(tmp);
                    Branch * ir_branch=new Branch(cmp->gettReg(),bodyBblock,nextBblock);
                    condBblock->addIR(ir_branch);
                    if(astnode->right->nodeType==COMPOUND_STMT)
                        generateIR(astnode->right, bodyBblock);
                    else{
                        Scope * tmpScope=new Scope();
                        tmpScope->setPreScope(scope);
                        scope=tmpScope;
                        generateIR(astnode->right, bodyBblock);
                        scope=scope->getpre();
                    }
                    IRs->addBlock(curFunc->getname(), nextBblock);
                    if(Jump * ir_last=dynamic_cast<Jump *>(bodyBblock->lastInstruction())){
                        if(ir_last->getBblock()==condBblock){
                            ;
                        }
                        else{
                            bodyBblock->addIR(ir_jumpCond);
                        }
                    }
                    else{
                        bodyBblock->addIR(ir_jumpCond);
                    }
                    continueBblock.pop_back();
                    generateIR(astnode->next, nextBblock);
                    
                }
           }
           else{
                if(astnode->left->ivalue==0){generateIR(astnode->next, curBblock);}
                else{
                    BasicBlock * bodyBblock=new BasicBlock("."+to_string(bblockNum++));
                    IRs->addBlock(curFunc->getname(), bodyBblock);
                    BasicBlock * nextBblock=new BasicBlock("."+to_string(bblockNum++));

                    continueBblock.push_back(bodyBblock);
                    //breakBblock.push_back(nextBblock);

                    Jump * ir_jumpBody=new Jump(bodyBblock);
                    curBblock->addIR(ir_jumpBody);
                    if(astnode->right->nodeType==COMPOUND_STMT)
                        generateIR(astnode->right, bodyBblock);
                    else{
                        Scope * tmpScope=new Scope();
                        tmpScope->setPreScope(scope);
                        scope=tmpScope;
                        generateIR(astnode->right, bodyBblock);
                        scope=scope->getpre();
                    }
                    if(Jump * ir_last=dynamic_cast<Jump *>(bodyBblock->lastInstruction())){
                        if(ir_last->getBblock()==bodyBblock){
                            ;
                        }
                        else{
                            bodyBblock->addIR(ir_jumpBody);
                        }
                    }
                    else{
                        bodyBblock->addIR(ir_jumpBody);
                    }
                    IRs->addBlock(curFunc->getname(), nextBblock);
                    continueBblock.pop_back();
                    generateIR(astnode->next, nextBblock);
                }
           }
           //continueBblock.pop_back();
           //breakBblock.pop_back();
            return NULL;
        }
            break;
        case IF_STMT://记得修剪优化语法树
        {
            past tmp=astnode;
            while(tmp->nodeType!=IF_STMT){
                astnode->if_cond=editWhileCond(astnode->if_cond);
                tmp=astnode->right;
            }
            astnode=shortCircuitDeleteAst(astnode);
            if(astnode->nodeType!=IF_STMT){
                Instruction * ir=generateIR(astnode, curBblock);
                return ir;
            }

            vector<vector<pair<past,BasicBlock *>>> ifConds;
            vector<pair<past,BasicBlock *>> ifBodys;
            get_ifConds_Bodys(astnode, ifConds, ifBodys);
            vector<vector<pair<past,BasicBlock *>>>::iterator ifCond=ifConds.begin();
            vector<pair<past,BasicBlock *>>::iterator ifBody=ifBodys.begin();
            if(ifBodys.size()>ifConds.size()){
                vector<pair<past,BasicBlock *>> tmp={ifBodys.back()};
                ifConds.push_back(tmp);
            }
            setFirstCondBblock(ifConds,ifBodys, curBblock);

            BasicBlock * nextBblock;
            

            //条件短路的删除
            //shortCircuitDelete(ifConds,ifBodys,curBblock);

            //setFirstCondBblock(ifConds,ifBodys,curBblock);
            
            //最后一个cond不是else且被短路过且只有一个cond情况下，nextbblock不需要新建
            if(ifConds.size()==1
                &&(*(ifConds.end()-1))[0].first!=(*(ifBodys.end()-1)).first
                &&(*(ifConds.end()-1))[0].second==(*(ifBodys.end()-1)).second){
                    nextBblock=(*(ifBodys.end()-1)).second;
                }
            //当cond被优化到只剩下一个else的时候不需要新建nextbblock
            //else if(ifConds.size()==1&&(*(ifConds.end()-1))[0].first==(*(ifBodys.end()-1)).first){
             //   nextBblock=curBblock;
            //}
            //整个ifelse都被优化掉时也不需要新建nextbblock
            //else if(ifConds.size()==0){
            //    nextBblock=curBblock;
            //}
            else{
                nextBblock=new BasicBlock("."+to_string(bblockNum++));
            }


            generateIR_IfConds(ifConds,ifBodys,nextBblock);
            generateIR_IfBodys(ifBodys,nextBblock);
            
            for(auto ifCond=ifConds.begin();ifCond!=ifConds.end();++ifCond,++ifBody){
                for(auto cond=ifCond->begin();cond!=ifCond->end();cond++){
                    if(ifCond==ifConds.begin()&&cond->second==curBblock){
                        continue;
                    }
                    if(cond->second==ifBody->second){
                        continue;
                    }
                    IRs->addBlock(curFunc->getname(), cond->second);
                }
                IRs->addBlock(curFunc->getname(), ifBody->second);
            }
            IRs->addBlock(curFunc->getname(), nextBblock);
            generateIR(astnode->next,nextBblock);
            return NULL;
        }
            

			break;
		case CONTINUE_STMT:{
            Jump * ir_jump=new Jump(*(continueBblock.end()-1));
            curBblock->addIR(ir_jump);
            //BasicBlock * newBblock=new BasicBlock("."+to_string(bblockNum++));
            //IRs->addBlock(curFunc->getname(), newBblock);
            //generateIR(astnode->next,newBblock);
            return ir_jump;
        }
			break;
		case BREAK_STMT:{
            Jump * ir_jump=new Jump(*breakBblock.end());
            curBblock->addIR(ir_jump);
            BasicBlock * newBblock=new BasicBlock("."+to_string(bblockNum++));
            IRs->addBlock(curFunc->getname(), newBblock);
            generateIR(astnode->next,newBblock);
            return ir_jump;
        }

			break;
		case RETURN_STMT:{
            Instruction * val=generateIR(astnode->left, curBblock);
            Instruction * tmpval=generateIR_Tran(curBblock,val,curFunc->getRetType());
            Ret *ir_ret;
            if(tmpConstant * retval=dynamic_cast<tmpConstant *>(tmpval)){
                
                ir_ret=new Ret(retval->getVal());
            }
            if(BinaryOperation * retval=dynamic_cast<BinaryOperation *>(tmpval)){
                ir_ret=new Ret(retval->gettReg());
            }
            if(TypeTran * retval=dynamic_cast<TypeTran *>(tmpval)){
                ir_ret=new Ret(retval->gettReg());
            }
            if(Load * retval=dynamic_cast<Load *>(tmpval)){
                ir_ret=new Ret(retval->gettReg());
            }
            curBblock->addIR(ir_ret);
            generateIR(astnode->next, curBblock);
            return ir_ret;
        }
			break;
		case INTEGER_LITERAL:{
            tmpConstant *tmp=new tmpConstant(Type(INT,0),astnode->ivalue);
            return tmp;
        }
			break;
		case FLOATING_LITERAL:{
            tmpConstant *tmp=new tmpConstant(Type(FLOAT,0),astnode->fvalue);
            return tmp;
        }
			break;
		case DECL_REF_EXPR:{//函数的DeclRefExpr在CallExpr中处理
            sVar * svar=scope->lookup(astnode->svalue);
            //生成IR
            if(svar->firstReg->type.isConst){
                if(svar->initVal!=NULL){
                    tmpConstant * ir_const=new tmpConstant(Type(svar->firstReg->type.type,0),svar->initVal);
                    return ir_const;
                }
                else{
                    Variable *tReg=new Variable(Type(svar->firstReg->type.type,0),"%"+to_string(regNum++),curFunc);
                    Load * ir_load=new Load(tReg,svar->firstReg);
                    curBblock->addIR(ir_load);
                    if(std::find(condDeclRef.begin(), condDeclRef.end(), astnode)!=condDeclRef.end()){
                        Variable * resultReg=new Variable(Type(I1,0),"%"+to_string(regNum++),curFunc);
                        Constant * zeroConst=new Constant(Type((tReg->type.type==FLOAT?FLOAT:INT),0),(float)0.0);
                        BinaryOperation* ir_cmpZero=new BinaryOperation(NOT_EQUAL,tReg,zeroConst,tReg->type.type==FLOAT?FCMP:ICMP,resultReg);
                        curBblock->addIR(ir_cmpZero);
                        return ir_cmpZero;
                    }
                    return ir_load;
                }
            }
            else{
                Variable *tReg=new Variable(Type(svar->firstReg->type.type,0),"%"+to_string(regNum++),curFunc);
                Load * ir_load=new Load(tReg,svar->firstReg);
                curBblock->addIR(ir_load);
                if(std::find(condDeclRef.begin(), condDeclRef.end(), astnode)!=condDeclRef.end()){
                        Variable * resultReg=new Variable(Type(I1,0),"%"+to_string(regNum++),curFunc);
                        Constant * zeroConst=new Constant(Type((tReg->type.type==FLOAT?FLOAT:INT),0),(float)0.0);
                        BinaryOperation* ir_cmpZero=new BinaryOperation(NOT_EQUAL,tReg,zeroConst,tReg->type.type==FLOAT?FCMP:ICMP,resultReg);
                        curBblock->addIR(ir_cmpZero);
                        return ir_cmpZero;
                    }
                return ir_load;
            }
        }
			break;
		case BINARY_OPERATOR:{
            Instruction* ir_result;
            astnode=editBinaryOper(astnode, scope);
            if(astnode->nodeType==BINARY_OPERATOR){
                ir_result=generateIR_BinaryOper(astnode, curBblock);
            }
            else{
                ir_result=generateIR(astnode,curBblock);
            }
            if(initListing==true){
                return ir_result;
            }
            generateIR(astnode->next,curBblock);
            return ir_result;
        }
			break;
		case INIT_LIST_EXPR:
            return NULL;
			break;
		case DECL_STMT:{
            Instruction * ir_decl=generateIR(astnode->left,curBblock);
            generateIR(astnode->next, curBblock);
            return ir_decl;
        }
			break;
		case VAR_DECL:{
            string varName;
            if(scope->isGlobal()){
                varName="@"+string(astnode->svalue);
                Variable * varible=new Variable(Type(astnode->type,getDimen(astnode->left),1),varName,curFunc);
                sVar* svar=new sVar(varible);
                if(astnode->right!=NULL)
                    svar->getInitVals(varible->type.arraysubs, astnode->right,scope);
                Constant * initValue;
                if(astnode->right==NULL)
                    initValue=new Constant(svar->firstReg->type,0);
                else
                    initValue=new Constant(svar->firstReg->type,svar->initVal);
                initValue->trans(varible->type);
                Global * ir_global=new Global(varible,initValue,svar->initVals);
                curBblock->addIR(ir_global);
                curFunc->addSymbols(varible);
                scope->addVar(astnode->svalue, svar);
                generateIR(astnode->next,curBblock);
                return ir_global;
            }
            else{
                varName="%"+std::to_string(regNum);
                regNum++;
                Variable * varible=new Variable(Type(astnode->type,getDimen(astnode->left),1),varName,curFunc);
                sVar * svar;
                svar=new sVar(varible);
                scope->addVar(astnode->svalue, svar);
                Alloca * ir_alloca=new Alloca(varible->type,varible);
                curBblock->addIR(ir_alloca);
                if(astnode->right==NULL){//声明变量但没有初始值
                    generateIR(astnode->next,curBblock);
                    return ir_alloca;;
                }
                else if(astnode->right->nodeType==INIT_LIST_EXPR){//局部数组的初始化
                    initListing=true;
                    generateIR_InitList(astnode->right,curBblock,0,varible);
                    initListing=false;
                }
                else{//用数组引用、变量引用、常量初始化
                    astnode->right=editBinaryOper(astnode->right, scope);
                    Instruction * ir_val= generateIR(astnode->right,curBblock);
                    ir_val=generateIR_Tran(curBblock,ir_val, varible->getType());
                    Store * ir_store=new Store(varible,getOpd(ir_val));
                    curBblock->addIR(ir_store);
                    if(tmpConstant * ir_const=dynamic_cast<tmpConstant*>(ir_val)){
                        Constant* initVal=new Constant(ir_const->getType(),ir_const->getVal()->val);
                        if(astnode->type==const_float||astnode->type==const_int) svar->initVal=initVal;
                    }
                    generateIR(astnode->next,curBblock);
                    return ir_alloca;
                }
            }
        }
            
			break;
		case UNARY_OPERATOR:{
            if(astnode->ivalue=='+'){
                Instruction * ir=generateIR(astnode->left, curBblock);
                generateIR(astnode->next, curBblock);
                return ir;}
            else if(astnode->ivalue=='-'){
                Instruction * ir=generateIR(astnode->left,curBblock);
                if(tmpConstant * val=dynamic_cast<tmpConstant *>(ir)){
                    val->getVal()->nega();
                    generateIR(astnode->next, curBblock);
                    return val;
                }
                else if(Load * val=dynamic_cast<Load *>(ir)){
                    Constant * zeroConst=new Constant(val->gettReg()->type,0);
                    Variable * resultVar=new Variable(val->gettReg()->type,"%"+to_string(regNum++),curFunc);
                    BinaryOperation * ir_bo=new  BinaryOperation(SUB,zeroConst,val->gettReg(),resultVar);
                    curBblock->addIR(ir_bo);
                    generateIR(astnode->next, curBblock);
                    return ir_bo;
                } 
            }
            else if(astnode->ivalue=='!'){
                Instruction * ir=generateIR(astnode->left,curBblock);
                if(tmpConstant * val=dynamic_cast<tmpConstant *>(ir)){
                    if(val->getVal()->isZero()){
                        tmpConstant * ir_tmp=new tmpConstant(Type(I1,0),1);
                        generateIR(astnode->next, curBblock);
                        return ir_tmp;
                    }
                    else{
                        tmpConstant * ir_tmp=new tmpConstant(Type(I1,0),0);
                        generateIR(astnode->next, curBblock);
                        return ir_tmp;
                    }
                }
                else if(Load * val=dynamic_cast<Load *>(ir)){
                    Constant* cmpZero=new Constant(Type(val->gettReg()->type.type,0),0);
                    if(val->gettReg()->type.type==INT){
                        Variable * result=new Variable(Type(I1,1),"%"+to_string(regNum++),curFunc);
                        BinaryOperation * ir_bo=new BinaryOperation(SNE,val->gettReg(),cmpZero,ICMP,result);
                        curBblock->addIR(ir_bo);
                        Variable * result2=new Variable(Type(I1,1),"%"+to_string(regNum++),curFunc);
                        Constant * trueConstant=new Constant(Type(I1,0),1);
                        BinaryOperation * ir_xor=new BinaryOperation(XOR,result,trueConstant,result2);
                        curBblock->addIR(ir_xor);
                        generateIR(astnode->next, curBblock);
                        return ir_xor;
                    }
                }
            }
        }
            
			break;
		case CALL_EXPR:{
            CallStmt * ir_call=new CallStmt(symbolTable->lookupFunc(astnode->right->svalue));
            past tmp=astnode->right;
            while(tmp!=NULL){
                ir_call->addParams(getParam(tmp,curBblock,0,symbolTable->lookupFunc(astnode->right->svalue)));
                tmp=tmp->next;
            }
            if(curFunc->getRetType().type!=VOID){
                Variable * retVal=new Variable(curFunc->getRetType(),
                                "%"+to_string(regNum++),curFunc);
                ir_call->setRetVal(retVal);
            }
            else{
                ir_call->setRetVal(NULL);
            }
            generateIR(astnode->next, curBblock);
            return ir_call;
        }

			break;
		case ARRAY_SUBSCRIPT_EXPR:{//局部数组用，返回load或能确定初始值的const变量的值
            if(arrayIsConst(astnode)){
                pair<int,sVar*> info=getArrayIndex(astnode, 0);
                if(info.second->initVals.find(info.first)==info.second->initVals.end()){
                    ;
                }
                else{
                    tmpConstant *ir_const=new tmpConstant(info.second->initVals[info.first]->getType(),
                                                            info.second->initVals[info.first]->val);
                    generateIR(astnode->next, curBblock);
                    return ir_const;
                }
            }
            Instruction * ir_tmp=generateIR_Param_Array(astnode,curBblock);
            if(GetElementPtr * ir_ptr=dynamic_cast<GetElementPtr *>(ir_tmp)){
                Variable * sReg=ir_ptr->getTptr();
                Variable * tReg=new Variable(Type(sReg->type.type,0),"%"+to_string(regNum++),curFunc);
                Load * ir_load=new Load(tReg,sReg);
                curBblock->addIR(ir_load);
                if(std::find(condDeclRef.begin(), condDeclRef.end(), astnode)!=condDeclRef.end()){
                        Variable * resultReg=new Variable(Type(I1,0),"%"+to_string(regNum++),curFunc);
                        Constant * zeroConst=new Constant(Type((tReg->type.type==FLOAT?FLOAT:INT),0),(float)0.0);
                        BinaryOperation* ir_cmpZero=new BinaryOperation(NOT_EQUAL,tReg,zeroConst,tReg->type.type==FLOAT?FCMP:ICMP,resultReg);
                        curBblock->addIR(ir_cmpZero);
                        return ir_cmpZero;
                }
                generateIR(astnode->next, curBblock);
                return ir_load;
            }
            
        }
			break;
		case PAREN_EXPR:{
            Instruction* ir_tmp= generateIR(astnode->left, curBblock);
            generateIR(astnode->next, curBblock);
            return ir_tmp;
        }
			break;
		case NULL_STMT:{
            generateIR(astnode->next, curBblock);
            return NULL;
        }
			break;
		default:
            break;
    };

}

past shortCircuitDeleteAst(past astnode){
    if(astnode==NULL){
        return NULL;
    }
    else if(astnode->nodeType!=IF_STMT){
        return astnode;
    }
    else if(astnode->if_cond->nodeType==INTEGER_LITERAL&&astnode->if_cond->ivalue==1){
        freeAst_remainNext(astnode->right);
        astnode->right=NULL;
        return astnode;
    }
    else if(astnode->if_cond->nodeType==INTEGER_LITERAL&&astnode->if_cond->ivalue==0){
        astnode->right=shortCircuitDeleteAst(astnode->right);
        past tmp=astnode->right;
        tmp->next=astnode->next;
        freeAst_remainNext(astnode->if_cond);
        free(astnode);
        return tmp;
    }
    else{
        astnode->right=shortCircuitDeleteAst(astnode->right);
        return astnode;
    }
}

void shortCircuitDelete(vector<vector<pair<past,BasicBlock *>>> &ifConds,
            vector<pair<past,BasicBlock *>> &ifBodys,BasicBlock * curBblock){
        for(auto ifCond=ifConds.begin();ifCond!=ifConds.end();){
            if(ifCond->size()==1&&(*ifCond)[0].first->nodeType==INTEGER_LITERAL&&(*ifCond)[0].first->ivalue==1){
                int index = std::distance(ifConds.begin(), ifCond);        
                if((*ifCond)[0].second==curBblock){
                   delete ifBodys[index].second;
                   ifBodys[index].second=curBblock;
                }
                else{
                    delete (*ifCond)[0].second;
                    (*ifCond)[0]=ifBodys[index];
                }
                    
                index=index+1;
                for(;ifConds.begin()+index!=ifConds.end();){
                    ifConds.erase(ifConds.begin()+index);
                    ifBodys.erase(ifBodys.begin()+index);
                }
                setFirstCondBblock(ifConds, ifBodys,curBblock);
                break;
        }
            else if(ifCond->size()==1&&(*ifCond)[0].first->nodeType==INTEGER_LITERAL&&(*ifCond)[0].first->ivalue==0){
                int index = std::distance(ifConds.begin(), ifCond);
                ifConds.erase(ifConds.begin() + index);
                ifBodys.erase(ifBodys.begin()+index);
                setFirstCondBblock(ifConds, ifBodys,curBblock);
            }
            else{
                ++ifCond;
            }
        }

}

void setFirstCondBblock(vector<vector<pair<past,BasicBlock *>>> &ifConds,
                        vector<pair<past,BasicBlock *>> &ifBodys,BasicBlock *curBblock){
    if(ifConds.size()==0)    
        return ;
    auto ifCond=ifConds.begin();
    //第二个cond是&&或||的情况
    if(ifCond->size()>1&&(*(*ifCond).begin()).second==(ifCond->begin()+1)->second){
        delete (ifCond->begin())->second;
        (*(*ifCond).begin()).second=(ifCond->begin()+1)->second=curBblock;
        return ;
    }
    //当cond优化到只剩下else时
    //第二个cond不是&&或||的情况但也不是1的情况下
    else if((*ifCond)[0].second!=curBblock
            &&!(ifCond->size()==1&&(*ifCond)[0].first->nodeType==INTEGER_LITERAL&&(*ifCond)[0].first->ivalue==1)){
        delete (ifCond->begin())->second;
        (*(*ifCond).begin()).second=curBblock;
    }

    if(ifCond->size()==1&&(*ifCond)[0].first->nodeType==INTEGER_LITERAL&&(*ifCond)[0].first->ivalue==1){
        int index = std::distance(ifConds.begin(), ifCond);
        delete (*ifCond)[0].second;     
        (*ifCond)[0].second=ifBodys[0].second=curBblock;
    }

}


BasicBlock * get_WhileCond(vector<pair<past,BasicBlock *>> &whileCond,past astnode,BasicBlock *preBblock){
    if(astnode==NULL)
        return NULL;
    BasicBlock *bblock;
    if(astnode->nodeType==DECL_REF_EXPR){
        condDeclRef.push_back(astnode);
        bblock=new BasicBlock("."+to_string(bblockNum++));
        whileCond.push_back(make_pair(astnode, bblock));
        return bblock;
    }
    else if(astnode->nodeType==BINARY_OPERATOR&&astnode->ivalue==AND){
        if(astnode->left->nodeType==DECL_REF_EXPR) condDeclRef.push_back(astnode->left);
        if(astnode->left->nodeType==DECL_REF_EXPR) condDeclRef.push_back(astnode->right);
        preBblock=get_WhileCond(whileCond, astnode->left,NULL);
        whileCond.push_back(make_pair(astnode, preBblock));
        bblock=get_WhileCond(whileCond,astnode->right,bblock);
        return bblock;
    }
    else if(astnode->nodeType==BINARY_OPERATOR&&astnode->ivalue==OR){
        preBblock=get_WhileCond(whileCond, astnode->left,NULL);
        whileCond.push_back(make_pair(astnode,preBblock));
        bblock=get_WhileCond(whileCond,astnode->right,bblock);
        return bblock;
    }
    //比较运算符的左右操作数只能是加减乘除取余这些二元运算
    else if(astnode->nodeType==BINARY_OPERATOR&&astnode->ivalue!=AND&&astnode->ivalue!=OR){
        bblock=new BasicBlock("."+to_string(bblockNum++));
        whileCond.push_back(make_pair(astnode, bblock));
        return bblock;
    }
    else{
        preBblock=get_WhileCond(whileCond, astnode->left,NULL);
        bblock=new BasicBlock("."+to_string(bblockNum++));
        whileCond.push_back(make_pair(astnode, bblock));
        get_WhileCond(whileCond, astnode->right,bblock);
        return bblock;
    }
}

BasicBlock * get_jumpBblock(vector<pair<past,BasicBlock *>> &whileCond,vector<pair<past, BasicBlock *>>::iterator it,BasicBlock *resultBblock){
    for(;it!=whileCond.end();++it){
        if((*it).first->ivalue==OR){
            vector<pair<past, BasicBlock *>>::iterator tmp=it+1;
            return (*tmp).second;
        }
    }
    return resultBblock;
}


BasicBlock * get_IfCond(vector<pair<past,BasicBlock *>> &ifCond,past astnode,BasicBlock *preBblock){
     if(astnode==NULL)
        return NULL;
    BasicBlock *bblock;
    if(astnode->nodeType==DECL_REF_EXPR){
        condDeclRef.push_back(astnode);
        bblock=new BasicBlock("."+to_string(bblockNum++));
        ifCond.push_back(make_pair(astnode, bblock));
        return bblock;
    }
    else if(astnode->nodeType==BINARY_OPERATOR&&astnode->ivalue==AND){
        if(astnode->left->nodeType==DECL_REF_EXPR) condDeclRef.push_back(astnode->left);
        if(astnode->left->nodeType==DECL_REF_EXPR) condDeclRef.push_back(astnode->right);
        preBblock=get_IfCond(ifCond, astnode->left,NULL);
        ifCond.push_back(make_pair(astnode, preBblock));
        bblock=get_IfCond(ifCond,astnode->right,bblock);
        return bblock;
    }
    else if(astnode->nodeType==BINARY_OPERATOR&&astnode->ivalue==OR){
        preBblock=get_IfCond(ifCond, astnode->left,NULL);
        ifCond.push_back(make_pair(astnode,preBblock));
        bblock=get_IfCond(ifCond,astnode->right,bblock);
        return bblock;
    }
    //比较运算符的左右操作数只能是加减乘除取余这些二元运算
    else if(astnode->nodeType==BINARY_OPERATOR&&astnode->ivalue!=AND&&astnode->ivalue!=OR){
        bblock=new BasicBlock("."+to_string(bblockNum++));
        ifCond.push_back(make_pair(astnode, bblock));
        return bblock;
    }
    else{
        preBblock=get_IfCond(ifCond, astnode->left,NULL);
        bblock=new BasicBlock("."+to_string(bblockNum++));
        ifCond.push_back(make_pair(astnode, bblock));
        get_IfCond(ifCond, astnode->right,bblock);
        return bblock;
    }
}


void get_ifConds_Bodys(past astnode,vector<vector<pair<past,BasicBlock *>>> &ifConds,vector<pair<past,BasicBlock *>> &ifBodys){
    if(astnode==NULL)
        return ;

    if(astnode->nodeType==IF_STMT&&astnode->if_cond->nodeType==INTEGER_LITERAL
        &&astnode->if_cond->ivalue==1){
        vector<pair<past,BasicBlock *>> ifCond;
        BasicBlock * newBblock=new BasicBlock("."+to_string(bblockNum++));
        pair<past,BasicBlock*> tmp=make_pair(astnode->if_cond,newBblock);
        ifCond.push_back(tmp);
        ifConds.push_back(ifCond);
        ifBodys.push_back(make_pair(astnode->left, newBblock));
    }
    else if(astnode->nodeType==IF_STMT){//正常的if
        vector<pair<past,BasicBlock *>> ifCond;
        (get_IfCond(ifCond, astnode->if_cond,NULL));
        ifConds.push_back(ifCond);
        BasicBlock * bodyBblock=new BasicBlock("."+to_string(bblockNum++));
        ifBodys.push_back(make_pair(astnode->left, bodyBblock));
    }
    else{
        BasicBlock * bodyBblock=new BasicBlock("."+to_string(bblockNum++));
        ifBodys.push_back(make_pair(astnode->left, bodyBblock));
    }
    //条件为1的if
    get_ifConds_Bodys(astnode->right, ifConds, ifBodys);
}

void generateIR_IfCond(vector<vector<pair<past,BasicBlock *>>> &ifConds,
                        vector<vector<pair<past,BasicBlock *>>>::iterator ifCond,
                        vector<pair<past,BasicBlock *>> &ifBodys,
                        BasicBlock *nextBblock){
    if((*ifCond).size()==1&&(*ifCond)[0].first->ivalue==1)
        return ;
    if((*ifCond)[0].first==(ifBodys)[0].first){
        return ;
    }
    for (auto it = (*ifCond).begin(); it != (*ifCond).end(); ++it) {
        switch((*it).first->ivalue){
            case AND:{
                BasicBlock * jumpBblock=get_if_And_falseBblock(ifConds,ifCond,it,nextBblock);
                Instruction * tmp=(*it).second->lastInstruction();
                BinaryOperation * cmp=dynamic_cast<BinaryOperation *>(tmp);
                vector<pair<past, BasicBlock *>>::iterator next=it+1;
                BasicBlock * nextCond=(*next).second;
                Branch * ir_branch=new Branch(cmp->gettReg(),nextCond,jumpBblock);
                (*it).second->addIR(ir_branch);
                break;
            }
            case OR:{
                Instruction * tmp=(*it).second->lastInstruction();
                BinaryOperation * cmp=dynamic_cast<BinaryOperation *>(tmp);
                vector<pair<past, BasicBlock *>>::iterator next=it+1;
                BasicBlock * nextCond=(*next).second;
                int index = std::distance(ifConds.begin(), ifCond);
                Branch * ir_branch=new Branch(cmp->gettReg(),ifBodys[index].second,nextCond);
                (*it).second->addIR(ir_branch);
                break;
                }    
                default:
                    generateIR((*it).first,(*it).second);
                    //IRs->addBlock(curFunc->getname(), (*it).second);
                    break;
        }
    }
    BasicBlock * lastBblock=(*((*ifCond).end()-1)).second;
    Instruction * tmp=lastBblock->lastInstruction();
    BinaryOperation * cmp=dynamic_cast<BinaryOperation *>(tmp);
    int index = std::distance(ifConds.begin(), ifCond);
    Branch* ir_branch;
    if(ifCond==ifConds.end()-1)
        ir_branch=new Branch(cmp->gettReg(),ifBodys[index].second,nextBblock);
    else
        ir_branch=new Branch(cmp->gettReg(),ifBodys[index].second,ifConds[index+1][0].second);
    lastBblock->addIR(ir_branch);
    return ;
}

BasicBlock * get_if_And_falseBblock(vector<vector<pair<past,BasicBlock *>>> &ifConds,
                                    vector<vector<pair<past,BasicBlock *>>>::iterator ifCond,
                                    vector<pair<past,BasicBlock *>>::iterator andCond,
                                    BasicBlock * nextBblock){

    //下一个||的下一个，没有就跳到nextCond
    for(;andCond!=(*ifCond).end();++andCond){
        if((*andCond).first->ivalue==OR){
            vector<pair<past, BasicBlock *>>::iterator tmp=andCond+1;
            return (*tmp).second;
        }
    }
    

    if(ifCond==ifConds.end()-1){
        return nextBblock;
    }
    /*
    if((*(ifCond+1))[0].first->nodeType!=IF_STMT){
        return (*(ifCond+1))[0].second;
    }
    */
    return (*(ifCond+1))[0].second;
}

void generateIR_IfConds(vector<vector<pair<past,BasicBlock *>>> &ifConds,
                        vector<pair<past,BasicBlock *>> &ifBodys,
                        BasicBlock *nextBblock){
    for (auto ifCond = ifConds.begin(); ifCond != ifConds.end();++ifCond) {
        if((*ifCond).size()==1&&(*ifCond)[0]==ifBodys.back())
            break;
        generateIR_IfCond(ifConds,ifCond,ifBodys,nextBblock);
    }
}

void generateIR_IfBodys(vector<pair<past,BasicBlock*>> &ifBodys,BasicBlock* nextBblock){
    for (auto ifBody = ifBodys.begin(); ifBody != ifBodys.end(); ++ifBody){
        generateIR(ifBody->first,ifBody->second);
        if(nextBblock==(*(ifBodys.end()-1)).second){
            ;
        }
        else{
            Jump* ir_jump=new Jump(nextBblock);
            ifBody->second->addIR(ir_jump);
        }
        
    }
    return ;
}

Instruction * generateIR_BinaryOper(past astnode,BasicBlock *curBblock){
    switch(astnode->ivalue){
        case LES_EQUAL:
        case GRT_EQUAL:
        case '<':
        case '>':
        case EQUAL:
        case NOT_EQUAL:
        /*
        {
            Instruction * left=generateIR(astnode->left, curBblock);
            Instruction * right=generateIR(astnode->right, curBblock);
            Type leftType;
            SymbolEntry * opd1,*opd2;
            int cmpType=ICMP;
            if(tmpConstant* leftIR=dynamic_cast<tmpConstant *>(left)){
                leftType=leftIR->getType();
                opd1=leftIR->getVal();
            }
            if(Load* leftIR=dynamic_cast<Load *>(left)){
                leftType=leftIR->getsReg()->getType();
                opd1=leftIR->getsReg();
            }
            Type rightType;
            if(tmpConstant* leftIR=dynamic_cast<tmpConstant *>(left)){
                rightType=leftIR->getType();
                opd2=leftIR->getVal();
            }
            if(Load* leftIR=dynamic_cast<Load *>(left)){
                rightType=leftIR->getsReg()->getType();
                opd2=leftIR->getsReg();
            }
            //tran,稍后加入I1类型的转换
            if(leftType.type==INT&&rightType.type==FLOAT){
                if(tmpConstant* leftIR=dynamic_cast<tmpConstant *>(left)){
                    leftIR->getVal()->sitofp(Type());
                }
                if(Load* leftIR=dynamic_cast<Load *>(left)){
                    Variable * newVar=new Variable(Type(FLOAT,1),"%"+to_string(regNum++),curFunc);
                    TypeTran * ir_tran=new TypeTran(newVar,leftIR->getsReg(),Type(FLOAT,0),SITOFP);
                    curBblock->addIR(ir_tran);
                    opd2=ir_tran->gettReg();
                }
                cmpType=FCMP;
            } 
            if(rightType.type==INT&&leftType.type==FLOAT){
                if(tmpConstant* rightIR=dynamic_cast<tmpConstant *>(right)){
                    rightIR->getVal()->sitofp(Type());
                }
                if(Load* rightIR=dynamic_cast<Load *>(right)){
                    Variable * newVar=new Variable(Type(FLOAT,0),"%"+to_string(regNum++),curFunc);
                    TypeTran * ir_tran=new TypeTran(newVar,rightIR->getsReg(),Type(FLOAT,0),SITOFP);
                    curBblock->addIR(ir_tran);
                    opd2=ir_tran->gettReg();
                }
                cmpType=FCMP;
            } 
            if(rightType.type==FLOAT&&leftType.type==FLOAT){
                cmpType=FCMP;
            }
            Variable * tReg=new Variable(Type(I1,1),"%"+to_string(regNum++),curFunc);
            BinaryOperation * ir_bo=new BinaryOperation(cmpType,opd1,opd2,getCmpOp(astnode->ivalue,leftType,rightType),tReg);
            curBblock->addIR(ir_bo);
            return ir_bo;
        }
        */
        {
            Instruction * ir_left=generateIR(astnode->left, curBblock);
            Instruction * ir_right=generateIR(astnode->right, curBblock);
            Type leftType=getOpdType(ir_left);
            Type rightType=getOpdType(ir_right);
            Type resultType=Type(I1,0);
            int cmpType=ICMP;
            if(leftType.type==FLOAT||rightType.type==FLOAT) cmpType=FCMP;
            if(leftType.type==INT&&rightType.type==FLOAT){
               ir_left=generateIR_Tran(curBblock,ir_left,rightType);
            } 
            if(rightType.type==INT&&leftType.type==FLOAT){
                ir_right=generateIR_Tran(curBblock,ir_right,leftType);
            } 
            SymbolEntry * opd1=getOpd(ir_left);
            SymbolEntry * opd2=getOpd(ir_right);
            Variable * tReg=new Variable(resultType,"%"+to_string(regNum++),curFunc);
            BinaryOperation * ir_bo=new BinaryOperation(astnode->ivalue,opd1,opd2,cmpType,tReg);
            curBblock->addIR(ir_bo);
            return ir_bo;
            break;
        }
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':{
            Instruction * ir_left=generateIR(astnode->left, curBblock);
            Instruction * ir_right=generateIR(astnode->right, curBblock);
            Type leftType=getOpdType(ir_left);
            Type rightType=getOpdType(ir_right);
            Type resultType=leftType;
            if(leftType.type==INT&&rightType.type==FLOAT){
               ir_left=generateIR_Tran(curBblock,ir_left,rightType);
               resultType=rightType;
            } 
            if(rightType.type==INT&&leftType.type==FLOAT){
                ir_right=generateIR_Tran(curBblock,ir_right,leftType);
                resultType=leftType;
            } 
            SymbolEntry * opd1=getOpd(ir_left);
            SymbolEntry * opd2=getOpd(ir_right);
            /*
            SymbolEntry * opd1,*opd2;
            if(tmpConstant* leftIR=dynamic_cast<tmpConstant *>(left)){
                leftType=leftIR->getType();
                opd1=leftIR->getVal();
            }
            if(Load* leftIR=dynamic_cast<Load *>(left)){
                leftType=leftIR->getsReg()->getType();
                opd1=leftIR->getsReg();
            }
            if(BinaryOperation * leftIR=dynamic_cast<BinaryOperation *>(left)){
                leftType=leftIR->gettReg()->getType();
                opd1=leftIR->gettReg();
            }
            
            
            if(tmpConstant* leftIR=dynamic_cast<tmpConstant *>(left)){
                rightType=leftIR->getType();
                opd2=leftIR->getVal();
            }
            if(Load* leftIR=dynamic_cast<Load *>(left)){
                rightType=leftIR->getsReg()->getType();
                opd2=leftIR->getsReg();
            }
            //tran
            if(leftType.type==INT&&rightType.type==FLOAT){
                if(tmpConstant* leftIR=dynamic_cast<tmpConstant *>(left)){
                    leftIR->getVal()->sitofp(Type());
                }
                if(Load* leftIR=dynamic_cast<Load *>(left)){
                    Variable * newVar=new Variable(Type(FLOAT,0),"%"+to_string(regNum++),curFunc);
                    TypeTran * ir_tran=new TypeTran(newVar,leftIR->getsReg(),Type(FLOAT,0),SITOFP);
                    curBblock->addIR(ir_tran);
                    opd2=ir_tran->gettReg();
                }
                
            } 
            if(rightType.type==INT&&leftType.type==FLOAT){
                if(tmpConstant* rightIR=dynamic_cast<tmpConstant *>(right)){
                    rightIR->getVal()->sitofp(Type());
                }
                if(Load* rightIR=dynamic_cast<Load *>(right)){
                    Variable * newVar=new Variable(Type(FLOAT,0),"%"+to_string(regNum++),curFunc);
                    TypeTran * ir_tran=new TypeTran(newVar,rightIR->getsReg(),Type(FLOAT,0),SITOFP);
                    curBblock->addIR(ir_tran);
                    opd2=ir_tran->gettReg();
                }
                
            } 
            */
            Variable * tReg=new Variable(resultType,"%"+to_string(regNum++),curFunc);
            BinaryOperation * ir_bo=new BinaryOperation(astnode->ivalue,opd1,opd2,tReg);
            curBblock->addIR(ir_bo);
            return ir_bo;
            break;
        }
        case '=':{
            
            Variable * lVal=generateIR_Lval(astnode->left,curBblock);
            Instruction * ir_right=generateIR(astnode->right, curBblock);
            Type leftType=lVal->getType();
            Type rightType=getOpdType(ir_right);
            Type resultType=leftType;
            ir_right=generateIR_Tran(curBblock,ir_right,leftType);
            resultType=leftType;
            SymbolEntry * opd2=getOpd(ir_right);
            Store* ir_store=new Store(lVal,opd2);
            curBblock->addIR(ir_store);
            return ir_store;

        }
        default: break;
        }

}

Variable * generateIR_Lval(past astnode,BasicBlock * curBblock){
    if(astnode->nodeType==DECL_REF_EXPR){
        sVar * svar=scope->lookup(astnode->svalue);
        return svar->firstReg;
    }
    if(astnode->nodeType==ARRAY_SUBSCRIPT_EXPR){
        Instruction * ir_tmp=generateIR_Param_Array(astnode,curBblock);
        if(GetElementPtr * ir_ptr=dynamic_cast<GetElementPtr *>(ir_tmp)){
            Variable * lval=ir_ptr->getTptr();
            return lval;
        }
    }
    
}

SymbolEntry * getOpd(Instruction * sir){
    SymbolEntry * opd;
    if(tmpConstant* tir=dynamic_cast<tmpConstant *>(sir)){
        opd=tir->getVal();
    }
    if(Load* tir=dynamic_cast<Load *>(sir)){
        opd=tir->gettReg();
    }
    if(BinaryOperation * tir=dynamic_cast<BinaryOperation *>(sir)){
        opd=tir->gettReg();
    }
    if(TypeTran * tir=dynamic_cast<TypeTran *>(sir)){
        opd=tir->gettReg();
    }
    return opd;
}

Type getOpdType(Instruction * sir){
    Type opdType;
    if(tmpConstant* tir=dynamic_cast<tmpConstant *>(sir)){
        opdType=tir->getType();
    }
    if(Load* tir=dynamic_cast<Load *>(sir)){
        opdType=tir->gettReg()->getType();
    }
    if(BinaryOperation * tir=dynamic_cast<BinaryOperation *>(sir)){
        opdType=tir->gettReg()->getType();
    }
    return opdType;
}

int getCmpOp(int op,Type leftType,Type rightType){
    if(op==LES_EQUAL){
        if(leftType.type==FLOAT) return OLE;
        if(leftType.type==INT&&rightType.type==INT) return SLE;
    }
    else if(op==GRT_EQUAL){
        if(leftType.type==FLOAT) return OGE;
        if(leftType.type==INT&&rightType.type==INT) return SGE;
    }
    else if(op==NOT_EQUAL){
        if(leftType.type==FLOAT) return ONE;
        if(leftType.type==INT&&rightType.type==INT) return SNE;
    }
    else if(op=='<'){
        if(leftType.type==FLOAT) return OLT;
        if(leftType.type==INT&&rightType.type==INT) return SLT;
    }
    else if(op=='>'){
        if(leftType.type==FLOAT) return OGT;
        if(leftType.type==INT&&rightType.type==INT) return SGT;
    }
    else if(op=='='){
        if(leftType.type==FLOAT) return OEQ;
        if(leftType.type==INT&&rightType.type==INT) return SEQ;
    }

}

vector<int> getDimen(past astnode){
    if(astnode==NULL){
        std::vector<int> emptyVector;
        return emptyVector;
    }
    vector<int> arraysubs;
    while(astnode!=NULL){
        astnode->left=editBinaryOper(astnode->left, scope);
        int dimen=calcDimen(astnode->left);
        arraysubs.push_back(dimen);
        astnode=astnode->right;
    }
    return arraysubs;
}

int calcDimen(past astnode){
    //数组引用的情况稍后加上
    if(astnode->nodeType==INTEGER_LITERAL){
        return astnode->ivalue;
    }
    else if(astnode->nodeType==DECL_REF_EXPR){
        return scope->getDimenInitVal(astnode->svalue);
    }
    else if(astnode->nodeType==BINARY_OPERATOR){
        int leftOpd=calcDimen(astnode->left);
        int rightOpd=calcDimen(astnode->right);
        switch(astnode->ivalue){
            case '+':return leftOpd+rightOpd;
                break;
            case '-':return leftOpd-rightOpd;
                break;
            case '*':return leftOpd*rightOpd;
                break;
            case '/':return leftOpd/rightOpd;
                break;
            case '%':return leftOpd%rightOpd;
                break;
            default:break;
        }
    }
}

void  generateIR_InitList(past astnode,BasicBlock * curBblock,int index,Variable * array){
    if(astnode==NULL)
        return ;
    if(astnode->nodeType!=INIT_LIST_EXPR){
        for(past tmp=astnode;tmp!=NULL;tmp=tmp->next){
    		Instruction * val=generateIR(tmp,curBblock);
            //多维数组
            if(array->getType().arraysubs.size()>1){
                Variable * castptr=new Variable(Type(array->getType().type,1),"%"+to_string(regNum++),curFunc);
                TypeTran * ir_tran=new TypeTran(array,castptr,castptr->getType(),BITCAST);
                curBblock->addIR(ir_tran);
                Variable * tptr=new Variable(castptr->getType(),"%"+to_string(regNum++),curFunc);
                GetPtr * ir_getptr=new GetPtr(castptr,index,tptr);
                curBblock->addIR(ir_getptr);
                Instruction * ir_storeVal=generateIR_Tran(curBblock, val, tptr->getType());
                if(tmpConstant * value=dynamic_cast<tmpConstant *>(ir_storeVal)){
                    if(array->getType().isConst){
                        sVar * svar=scope->lookup(array);
                        Constant *initval=new Constant(value->getType(),value->getVal()->val);
                        svar->initVals[index]=initval;
                    }
                    Store * ir_store=new Store(tptr,value->getVal());
                    curBblock->addIR(ir_store);
                }
                else if(Load * value=dynamic_cast<Load *>(ir_storeVal)){
                    Store * ir_store=new Store(tptr,value->gettReg());
                    curBblock->addIR(ir_store);
                }
                else if(TypeTran * value=dynamic_cast<TypeTran *>(ir_storeVal)){
                    Store * ir_store=new Store(tptr,value->gettReg());
                    curBblock->addIR(ir_store);
                }
                index++;
            }
            //一维数组
            else{
                Variable * tptr=new Variable(array->getType().getPtrType(),"%"+to_string(regNum++),curFunc);
                GetElementPtr * ir_GEptr=new GetElementPtr(array,index,tptr);
                curBblock->addIR(ir_GEptr);
                Instruction * ir_storeVal=generateIR_Tran(curBblock, val, tptr->getType());
                if(tmpConstant * value=dynamic_cast<tmpConstant *>(ir_storeVal)){
                    if(array->getType().isConst){
                        sVar * svar=scope->lookup(array);
                        Constant *initval=new Constant(value->getType(),value->getVal()->val);
                        svar->initVals[index]=initval;
                    }
                    Store * ir_store=new Store(tptr,value->getVal());
                    curBblock->addIR(ir_store);
                }
                else if(Load * value=dynamic_cast<Load *>(ir_storeVal)){
                    Store * ir_store=new Store(tptr,value->gettReg());
                    curBblock->addIR(ir_store);
                }
                else if(TypeTran * value=dynamic_cast<TypeTran *>(ir_storeVal)){
                    Store * ir_store=new Store(tptr,value->gettReg());
                    curBblock->addIR(ir_store);
                }
                index++;
            }
        }
        return ;
	}
    else{
        int i=array->type.arraysubs.size()-astnode->dimen-1;
        int tmpindex=1;
        for(;i<=array->type.arraysubs.size()-1;i++)
            tmpindex*=array->type.arraysubs[i];
        generateIR_InitList(astnode->left,curBblock,index,array);
        generateIR_InitList(astnode->next,curBblock,index+tmpindex,array);
    }
}

SymbolEntry * getParam(past astnode,BasicBlock * curBblock,int index,Function * func){
    if(astnode->nodeType==DECL_REF_EXPR){
        sVar * svar=scope->lookup(astnode->svalue);
            //生成IR
        Variable *tReg=new Variable(svar->firstReg->type,"%"+to_string(regNum++),curFunc);
        Load * ir_load=new Load(tReg,svar->firstReg);
        curBblock->addIR(ir_load);
        if(func->formalParameters[index].type==FLOAT
            &&svar->firstReg->type.type==INT){
            Variable * tmpReg=new Variable(Type(FLOAT,1),"%"+to_string(regNum++),curFunc);
            TypeTran *ir_tran=new TypeTran(tReg,tmpReg,Type(INT,0),SITOFP);
            curBblock->addIR(ir_tran);
            return tmpReg;
        }
        if(func->formalParameters[index].type==INT
            &&svar->firstReg->type.type==FLOAT){
            Variable * tmpReg=new Variable(Type(INT,1),"%"+to_string(regNum++),curFunc);
            TypeTran *ir_tran=new TypeTran(tReg,tmpReg,Type(FLOAT,0),FPTOSI);
            curBblock->addIR(ir_tran);
            return tmpReg;
        }
        return svar->firstReg;
    }
    else if(astnode->nodeType==INTEGER_LITERAL){
        Constant * intConst=new Constant(Type(INT,0),astnode->ivalue);
        if(func->formalParameters[index].type==FLOAT){
            intConst->sitofp(Type(FLOAT,0));
        }
        return intConst;
    }
    else if(astnode->nodeType==FLOATING_LITERAL){
        Constant * floatConst=new Constant(Type(FLOAT,0),astnode->fvalue);
        if(func->formalParameters[index].type==FLOAT){
            floatConst->fptosi(Type(INT,0));
        }
        return floatConst;
    }
    else if(astnode->nodeType==ARRAY_SUBSCRIPT_EXPR){
        Instruction* ir_tmp=generateIR_Param_Array(astnode,curBblock);
        if(GetElementPtr * ir_ptr=dynamic_cast<GetElementPtr *>(ir_tmp)){
            return ir_ptr->getTptr();
        }
    }
}

Instruction * generateIR_Param_Array(past astnode,BasicBlock * curBblock){
    if(astnode->nodeType!=ARRAY_SUBSCRIPT_EXPR)
        return NULL;
    if(astnode->right->nodeType==DECL_REF_EXPR){
        sVar * svar=scope->lookup(astnode->right->svalue);
        Variable *tReg=new Variable(svar->firstReg->type.getPtrType(),"%"+to_string(regNum++),curFunc);
        GetElementPtr * ir_ptr=new GetElementPtr(svar->firstReg,astnode->left->ivalue,tReg);
        curBblock->addIR(ir_ptr);
        return ir_ptr;
    }
    else{
        Instruction * ir_tmp=generateIR_Param_Array(astnode->right,curBblock);
        if(GetElementPtr * ir_prePtr=dynamic_cast<GetElementPtr *>(ir_tmp)){
            Variable *tReg=new Variable(ir_prePtr->getTptr()->type.getPtrType(),"%"+to_string(regNum++),curFunc);
            GetElementPtr * ir_ptr=new GetElementPtr(ir_prePtr->getTptr(),astnode->left->ivalue,tReg);
            curBblock->addIR(ir_ptr);
            return ir_ptr;
        }
    }
}

past editWhileCond(past astnode){
    if(astnode==NULL){
        return NULL;
    }
    astnode->left=editWhileCond(astnode->left);
    astnode->right=editWhileCond(astnode->right);
    if(astnode->nodeType==BINARY_OPERATOR&&astnode->ivalue==AND){
        if(astnode->left->nodeType==DECL_REF_EXPR||astnode->left->nodeType==ARRAY_SUBSCRIPT_EXPR){
            astnode->left=editBinaryOper(astnode->left, scope);
        }
        
        if(astnode->right->nodeType==DECL_REF_EXPR||astnode->right->nodeType==ARRAY_SUBSCRIPT_EXPR){
            astnode->right=editBinaryOper(astnode->right, scope);
        }
        
        if(isTrue(astnode->left)){
            free(astnode->left);
            past tmp=astnode->right;
            free(astnode);
            return tmp;
        }
        else if(isTrue(astnode->right)){
            free(astnode->right);
            past tmp=astnode->left;
            free(astnode);
            return tmp;
        }
        else if(isFalse(astnode->left)){
            free(astnode->right);
            past tmp=astnode->left;
            free(astnode);
            return tmp;
        }
        else if(isFalse(astnode->right)){
            free(astnode->left);
            past tmp=astnode->right;
            free(astnode);
            return tmp;
        }
    }
    else if(astnode->nodeType==BINARY_OPERATOR&&astnode->ivalue==OR){
        if(astnode->left->nodeType==DECL_REF_EXPR||astnode->left->nodeType==ARRAY_SUBSCRIPT_EXPR){
            astnode->left=editBinaryOper(astnode->left, scope);
        }
        
        if(astnode->right->nodeType==DECL_REF_EXPR||astnode->right->nodeType==ARRAY_SUBSCRIPT_EXPR){
            astnode->right=editBinaryOper(astnode->right, scope);
        }
        

        if(isFalse(astnode->left)){
            free(astnode->left);
            past tmp=astnode->right;
            free(astnode);
            return tmp;
        }
        else if(isFalse(astnode->right)){
            free(astnode->right);
            past tmp=astnode->left;
            free(astnode);
            return tmp;
        }
        else if(isTrue(astnode->left)){
            free(astnode->right);
            past tmp=astnode->left;
            free(astnode);
            return tmp;
        }
        else if(isTrue(astnode->right)){
            free(astnode->left);
            past tmp=astnode->right;
            free(astnode);
            return tmp;
        }
    }
    else if(astnode->nodeType==BINARY_OPERATOR){
        return editBinaryOper(astnode, scope);
    }
    return astnode;
    
}

bool astnodeIsConst(past astnode,Scope* scope){
    if(astnode->nodeType==FLOATING_LITERAL||astnode->nodeType==INTEGER_LITERAL)
        return true;
    else if(astnode->nodeType==ARRAY_SUBSCRIPT_EXPR){
        return arrayIsConst(astnode);
    }
    else {
        return false;
    }
}

int getTypeAST(past astnode,Scope * scope){
    int type;
    if(astnode->nodeType==DECL_REF_EXPR){
        type=scope->lookup(astnode->svalue)->firstReg->type.type;
    }
    else if(astnode->nodeType==INTEGER_LITERAL){
        type=INT;
    }
    else if(astnode->nodeType==FLOATING_LITERAL){
        type=FLOAT;
    }
    return type;
}

past calcConstAst(past astnode,Scope * scope){
    int type1,type2;
    past opdNode1=astnode->left;
    past opdNode2=astnode->right;
    type1=getTypeAST(opdNode1, scope);
    type2=getTypeAST(opdNode2,scope);
    past newNode;
    switch(astnode->ivalue){
        case '+':{
            if(type1==INT&&type2==INT){
                newNode=newIntLiteral(opdNode1->ivalue+opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==INT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->ivalue+opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==INT){
                newNode=newFloatLiteral(opdNode1->fvalue+opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->fvalue+opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            break;
        }
        case '-':{
            if(type1==INT&&type2==INT){
                newNode=newIntLiteral(opdNode1->ivalue-opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==INT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->ivalue-opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==INT){
                newNode=newFloatLiteral(opdNode1->fvalue-opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->fvalue-opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            break;
        }
        case '*':{
            if(type1==INT&&type2==INT){
                newNode=newIntLiteral(opdNode1->ivalue*opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==INT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->ivalue*opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==INT){
                newNode=newFloatLiteral(opdNode1->fvalue*opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->fvalue*opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            break;
        }
        case '/':{
            if(type1==INT&&type2==INT){
                newNode=newIntLiteral(opdNode1->ivalue/opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==INT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->ivalue/opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==INT){
                newNode=newFloatLiteral(opdNode1->fvalue/opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->fvalue/opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            break;
        }
        case '%':{
            if(type1==INT&&type2==INT){
                newNode=newIntLiteral(opdNode1->ivalue%opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            break;
        }
        case '<':{
            if(type1==INT&&type2==INT){
                newNode=newIntLiteral(opdNode1->ivalue<opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==INT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->ivalue<opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==INT){
                newNode=newFloatLiteral(opdNode1->fvalue<opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->fvalue<opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            break;
        }
        case '>':{
            if(type1==INT&&type2==INT){
                newNode=newIntLiteral(opdNode1->ivalue>opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==INT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->ivalue>opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==INT){
                newNode=newFloatLiteral(opdNode1->fvalue>opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->fvalue>opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            break;
        }
        case LES_EQUAL:{
            if(type1==INT&&type2==INT){
                newNode=newIntLiteral(opdNode1->ivalue<=opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==INT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->ivalue<=opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==INT){
                newNode=newFloatLiteral(opdNode1->fvalue<=opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->fvalue<=opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            break;
        }
        case GRT_EQUAL:{
            if(type1==INT&&type2==INT){
                newNode=newIntLiteral(opdNode1->ivalue>=opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==INT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->ivalue>=opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==INT){
                newNode=newFloatLiteral(opdNode1->fvalue>=opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->fvalue>=opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            break;
        }
        case EQUAL:{
            if(type1==INT&&type2==INT){
                newNode=newIntLiteral(opdNode1->ivalue==opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==INT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->ivalue==opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==INT){
                newNode=newFloatLiteral(opdNode1->fvalue==opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->fvalue==opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            break;
        }
        case NOT_EQUAL:{
            if(type1==INT&&type2==INT){
                newNode=newIntLiteral(opdNode1->ivalue!=opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==INT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->ivalue!=opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==INT){
                newNode=newFloatLiteral(opdNode1->fvalue!=opdNode2->ivalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            if(type1==FLOAT&&type2==FLOAT){
                newNode=newFloatLiteral(opdNode1->fvalue<opdNode2->fvalue);
                free(opdNode1);free(opdNode2);
                newNode->next=astnode->next;
                free(astnode);
                return newNode;
            }
            break;
        }
        default: break;
    }
}

past editBinaryOper(past astnode,Scope* scope){
    if(astnode==NULL)
        return NULL;
    if(astnode->nodeType==DECL_REF_EXPR){
        past newnode;
        sVar * svar=scope->lookup(astnode->svalue);
        if(svar==NULL){
            return astnode;
        }
        if(svar->firstReg->getType().isConst){
            if(svar->firstReg->getType().type==FLOAT){
                newnode=newFloatLiteral(svar->initVal->val.fval);
                newnode->next=astnode->next;
            }
            if(svar->firstReg->getType().type==INT){
                newnode=newIntLiteral(svar->initVal->val.ival);
                newnode->next=astnode->next;
            }
            free(astnode);
            return newnode;
        }
        return astnode;
    }
    else if(astnode->nodeType==ARRAY_SUBSCRIPT_EXPR){
        past newnode;
        if(arrayIsConst(astnode)){
            pair<int,sVar*> info=getArrayIndex(astnode, 0);
            if(info.second->initVals.find(info.first)==info.second->initVals.end()){
                return astnode;
            }
            else{
                if(info.second->firstReg->type.type==INT){
                    newnode=newIntLiteral(info.second->initVals[info.first]->val.ival);
                    newnode->next=astnode->next;
                    freeAst_remainNext(astnode);
                }
                else if(info.second->firstReg->type.type==FLOAT){
                    newnode=newFloatLiteral(info.second->initVals[info.first]->val.fval);
                    newnode->next=astnode->next;
                    freeAst_remainNext(astnode);
                }
                return newnode;
            }
        }
        return astnode;
    }
    else if(astnode->nodeType==BINARY_OPERATOR){
        astnode->left=editBinaryOper(astnode->left,scope);
        astnode->right=editBinaryOper(astnode->right,scope);
        if(astnodeIsConst(astnode->left,scope)&&astnodeIsConst(astnode->right,scope)){
            past newnode=calcConstAst(astnode,scope);
            return newnode;
        }
        else {
            return astnode;
        }
    }
    else if(astnode->nodeType==PAREN_EXPR){
        astnode->left=editBinaryOper(astnode->left, scope);
    }
    else{
        return astnode;
    }
}

bool isTrue(past astnode){
    if(astnode->nodeType==INTEGER_LITERAL)
        if(astnode->ivalue!=0)
            return true;
    if(astnode->nodeType==FLOATING_LITERAL)
        if(astnode->fvalue!=0)
            return true;
    return false;
}

bool isFalse(past astnode){
    if(astnode->nodeType==INTEGER_LITERAL)
        if(astnode->ivalue==0)
            return true;
    if(astnode->nodeType==FLOATING_LITERAL)
        if(astnode->fvalue==0)
            return true;
    return false;
}

Instruction * generateIR_Tran(BasicBlock* bblock,Instruction * sir,Type ttype){
    if(tmpConstant * ir_const=dynamic_cast<tmpConstant *>(sir)){
        if(ir_const->getVal()->getType().type==INT&&ttype.type==FLOAT){
            ir_const->getVal()->sitofp(Type(FLOAT,0));
            return ir_const;
        }
        else if(ir_const->getVal()->getType().type==INT&&ttype.type==I1){
            ir_const->getVal()->sitobo(Type(I1,0));
            return ir_const;
        }
        else if(ir_const->getVal()->getType().type==FLOAT&&ttype.type==INT){
            ir_const->getVal()->fptosi(Type(INT,0));
            return ir_const;
        }
        else if(ir_const->getVal()->getType().type==FLOAT&&ttype.type==I1){
            ir_const->getVal()->fptobo(Type(I1,0));
            return ir_const;
        }
        else if(ir_const->getVal()->getType().type==I1&&ttype.type==INT){
            ir_const->getVal()->botosi(Type(INT,0));
            return ir_const;
        }
        else if(ir_const->getVal()->getType().type==I1&&ttype.type==FLOAT){
            ir_const->getVal()->botofp(Type(INT,0));
            return ir_const;
        }
        return sir;
        
    }
    if(Load * ir_var=dynamic_cast<Load *>(sir)){
        if(ir_var->gettReg()->getType().arraysubs.size()!=0){

        }
        else{
            if(ir_var->gettReg()->getType().type==INT&&ttype.type==FLOAT){
                Variable * newvar=new Variable(Type(FLOAT,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,SITOFP);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==INT&&ttype.type==I1){
                Variable * newvar=new Variable(Type(I1,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,TRUNC);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==FLOAT&&ttype.type==INT){
                Variable * newvar=new Variable(Type(INT,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,FPTOSI);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==FLOAT&&ttype.type==I1){
                Variable * newvar=new Variable(Type(I1,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,TRUNC);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==I1&&ttype.type==INT){
                Variable * newvar=new Variable(Type(INT,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,ZEXT);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==I1&&ttype.type==FLOAT){
                Variable * newvar=new Variable(Type(FLOAT,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,ZEXT);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==INT&&ttype.type==FLOAT){
                Variable * newvar=new Variable(Type(FLOAT,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,SITOFP);
                bblock->addIR(tir);
                return tir;
            }
            return sir;
        }
    }
    if(BinaryOperation * ir_var=dynamic_cast<BinaryOperation *>(sir)){
        if(ir_var->gettReg()->getType().arraysubs.size()!=0){

        }
        else{
            if(ir_var->gettReg()->getType().type==INT&&ttype.type==FLOAT){
                Variable * newvar=new Variable(Type(FLOAT,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,SITOFP);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==INT&&ttype.type==I1){
                Variable * newvar=new Variable(Type(I1,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,TRUNC);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==FLOAT&&ttype.type==INT){
                Variable * newvar=new Variable(Type(INT,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,FPTOSI);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==FLOAT&&ttype.type==I1){
                Variable * newvar=new Variable(Type(I1,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,TRUNC);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==I1&&ttype.type==INT){
                Variable * newvar=new Variable(Type(INT,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,ZEXT);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==I1&&ttype.type==FLOAT){
                Variable * newvar=new Variable(Type(FLOAT,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,ZEXT);
                bblock->addIR(tir);
                return tir;
            }
            if(ir_var->gettReg()->getType().type==INT&&ttype.type==FLOAT){
                Variable * newvar=new Variable(Type(FLOAT,0),"%"+to_string(regNum++),curFunc);
                TypeTran * tir=new TypeTran(ir_var->gettReg(),newvar,newvar->type,SITOFP);
                bblock->addIR(tir);
                return tir;
            }
            return sir;
        }
    }

    return sir;
}

pair<int,sVar *> getArrayIndex(past astnode,int antiDimen){
    if(astnode->right->nodeType==ARRAY_SUBSCRIPT_EXPR){
        pair<int,sVar *> info=getArrayIndex(astnode->right, antiDimen+1);
        int index=astnode->left->ivalue;
        int basicIndex=1;
        for(int i=info.second->firstReg->type.arraysubs.size()-antiDimen;
            i<=info.second->firstReg->type.arraysubs.size()-1;i++){
            basicIndex*=info.second->firstReg->type.arraysubs[i];
        }
        return make_pair(index*basicIndex+info.first,info.second);
    }
    else if(astnode->right->nodeType==DECL_REF_EXPR){
        int basicIndex=1;
        sVar * svar=scope->lookup(astnode->right->svalue);
        int index=astnode->left->ivalue;
        for(int i=svar->firstReg->getType().arraysubs.size()-antiDimen;
            i<=svar->firstReg->getType().arraysubs.size()-1;i++){
            basicIndex*=svar->firstReg->getType().arraysubs[i];
        }
        return std::make_pair(index*basicIndex,svar);
    }
}

bool arrayIsConst(past astnode){
    if(astnode->right->nodeType==ARRAY_SUBSCRIPT_EXPR){
        return arrayIsConst(astnode->right);
    }
    else if(astnode->right->nodeType==DECL_REF_EXPR){
        sVar * svar=scope->lookup(astnode->right->svalue);
        return svar->firstReg->type.isConst;
    }
}

void freeAst_remainNext(past astnode){
    if(astnode==NULL)
        return ;
    freeAst_remainNext(astnode->left);
    freeAst_remainNext(astnode->right);
    free(astnode);
    return ;
}

void dumpIR(IR * IRs){
    std::ofstream file("ir.txt");
    for (auto bblocks = IRs->irs.begin(); bblocks != IRs->irs.end(); ++bblocks) {
        file<<"define"<<" "<<symbolTable->lookupFunc(bblocks->first)->getRetType().TypeToString()
            <<" "<<bblocks->first<<"(";
            if(symbolTable->lookupFunc(bblocks->first)->formalParameters.size()==0){
                file<<")";
            }
            for(auto param=symbolTable->lookupFunc(bblocks->first)->formalParameters.begin();
                param!=symbolTable->lookupFunc(bblocks->first)->formalParameters.end();++param){
                if(param==symbolTable->lookupFunc(bblocks->first)->formalParameters.end()-1){
                    file<<param->TypeToString()<<")";
                }
                else{
                    file<<param->TypeToString()<<","<<" ";
                }
            }
            file<<" "<<"{"<<"\n";
        for(auto bblock=(*bblocks).second.begin();bblock!=(*bblocks).second.end();++bblock){
            file<<(*bblock)->bblockName()<<" "<<":"<<"\n";
            for(auto ir=(*bblock)->instructions.begin();ir!=(*bblock)->instructions.end();++ir){
                if(Load * ir_load=dynamic_cast<Load *>(*ir)){
                    file<<"  "<<ir_load->gettReg()->varname()<<" "<<"="
                        <<" "<<"load"<<" "<<ir_load->gettReg()->type.TypeToString()
                        <<","<<" "<<ir_load->getsReg()->type.TypeToString()<<" "
                        <<ir_load->getsReg()->varname()<<'\n';
                }
                else if(Alloca * ir_alloca=dynamic_cast<Alloca *>(*ir)){
                    Type tmpType=ir_alloca->gettReg()->type;
                    tmpType.ptrDimen=0;
                    file<<"  "<<ir_alloca->gettReg()->varname()<<" "<<"="<<" "
                        <<"alloca"<<" "<<tmpType.TypeToString()
                        <<'\n';
                }
                else if(Store * ir_store=dynamic_cast<Store *>(*ir)){
                    file<<"  "<<"store"<<" "<<ir_store->getsReg()->SymToString()
                        <<","<<" "<<ir_store->gettReg()->SymToString()<<"\n";
                }
                else if(Branch * ir_br=dynamic_cast<Branch *>(*ir)){
                    file<<"  "<<"br"<<" "<<ir_br->getReg()->SymToString()
                        <<","<<" "<<"label"<<" "<<ir_br->gettrueBlock()->bblockName()
                        <<","<<" "<<ir_br->getfalseBlock()->bblockName()<<"\n";
                }
                else if(Jump * ir_jump=dynamic_cast<Jump *>(*ir)){
                    file<<"  "<<"br"<<" "<<"label"<<" "
                        <<ir_jump->getBblock()->bblockName()<<"\n";
                }
                else if(Ret * ir_ret=dynamic_cast<Ret *>(*ir)){
                    file<<"  "<<"ret"<<" "<<ir_ret->getVal()->SymToString()
                        <<"\n";
                }
                else if(TypeTran * ir_tran=dynamic_cast<TypeTran *>(*ir)){
                    file<<"  "<<ir_tran->gettReg()->varname()<<" "<<"="<<" "
                        <<ir_tran->IRToString()<<" "<<ir_tran->getsReg()->SymToString()
                        <<" "<<"to"<<" "<<ir_tran->tType.TypeToString()
                        <<"\n";
                }
                else if(GetElementPtr * ir_ptr=dynamic_cast<GetElementPtr *>(*ir)){
                    file<<"  "<<ir_ptr->getTptr()->varname()<<" "<<"="<<" "
                        <<"getelementptr"<<" "<<ir_ptr->getSptr()->SymToString()<<","<<" "
                        <<to_string(ir_ptr->offset)<<"\n";
                }
                else if(GetPtr * ir_ptr=dynamic_cast<GetPtr *>(*ir)){
                    file<<"  "<<ir_ptr->getTptr()->varname()<<" "<<"="<<" "
                        <<"getptr"<<" "<<ir_ptr->getSptr()->SymToString()<<","<<" "
                        <<to_string(ir_ptr->offset)<<"\n";
                }
                else if(CallStmt * ir_call=dynamic_cast<CallStmt *>(*ir)){
                    if(ir_call->getFunc()->getRetType().type==VOID){
                        file<<"  "<<"call"<<" "<<"void"<<" "<<ir_call->getFunc()->getname()
                            <<"(";
                        for(auto it:ir_call->params){
                            if(it!=*(ir_call->params.end()-1))
                                file<<(*it).SymToString()<<","<<" ";
                            else
                                file<<it->SymToString()<<")"<<"\n";
                        }
                    }
                }
                else if(BinaryOperation * ir_bo=dynamic_cast<BinaryOperation *>(*ir)){
                    file<<"  "<<ir_bo->gettReg()->varname()<<" "<<"="<<" "
                        <<ir_bo->OpToString()<<" "<<ir_bo->getOpd1()->SymToString()<<","
                        <<" "<<ir_bo->getOpd2()->SymToString()<<"\n";
                }
                else if(Phi * ir_phi=dynamic_cast<Phi *>(*ir)){
                    file<<"  "<<ir_phi->gettReg()->varname()<<" "<<"="<<" "<<"phi"
                    <<" "<<ir_phi->gettReg()->getType().TypeToString()<<" ";
                    for (auto&  phiSym: ir_phi->phiTable){
                        if(phiSym == *std::prev(ir_phi->phiTable.end())){
                            file<<"["<<phiSym.second->SymToString()<<","<<" "
                                <<phiSym.first->bblockName()<<"]";
                            break;
                        }
                        file<<"["<<phiSym.second->SymToString()<<","<<" "
                        <<phiSym.first->bblockName()<<"]"<<","<<" ";
                        
                    }
                    file<<"\n";
                }
            }
            file<<"\n";
        }
        file<<"}"<<"\n";
    }
    file.close();
}

