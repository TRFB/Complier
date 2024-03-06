#include"ast.hpp"
#include <cstddef>
#include <exception>
#include <functional>
#include <string>
#include "parser.tab.hh"
#include"symtable.hpp"
#include"ir.hh"
//while语法树修剪优化
//基本块自我分裂

IR* IRs=new IR();
Function *curFunc=new Function(string("0GLOBAL"),Type(VOID)); 
Scope * globalScope=new Scope();
Scope * scope = globalScope;
int regNum=0;
int bblockNum=0;

BasicBlock * resultBblock=NULL;
BasicBlock * breakBblock=NULL;
BasicBlock * continueBblock=NULL;
Phi * conditionPhi=NULL;

int tmpRegNum=0;

Alloca::Alloca(Type type,Variable * sReg)
    : type(type), sReg(sReg){
    if((sReg->getType()).arraysubs.size()!=0){
        alignNum=(sReg->getType()).arraysubs[0]*4;
    }
    else {
        alignNum=4;
    }
}

void Phi::addPhiSym(SymbolEntry * var,BasicBlock* bblock){
    phiTable[bblock]=var;
}

BasicBlock * generateIR(past astnode);
Instruction * generateIR_WhileCond(past astnode);
BasicBlock * generateIR_IfBody(past astnode);
BasicBlock * generateIR_IfCond(past astnode);
BasicBlock * generateIR_IfStmt(past astnode);

BasicBlock * generateIR(past astnode,BasicBlock * curBblock){
    static Function *curFunc=new Function(string("0GLOBAL"),Type(VOID)); 
    static Scope * globalScope=new Scope();
    static Scope * scope = globalScope;
    static int regNum=0;
    
    static int tmpRegNum=0;
    if(astnode==NULL){
        return NULL;
    }
    switch(astnode->nodeType){
        case TRANSLATION_UNIT:{
            generateIR(astnode->left);

            return NULL;
            }
            break;
        case FUNCTION_DECL:{
            //建立新的基本块
            BasicBlock * bblock=new BasicBlock("."+to_string(bblockNum++));
            curBblock=bblock;
            //设置作用域与形参
            regNum=0;
            Function * function= new Function(astnode->svalue,astnode->type);
            curFunc=function;
            IRs->addBlock(curFunc->getname(), curBblock);
            Scope * tmpScope=new Scope();
            tmpScope->setPreScope(scope);
            scope=tmpScope;
            past tmp=astnode->left;
            while(tmp!=NULL){
                (*function).addParm(Type(tmp->type,tmp->arraysubs));
                tmp=tmp->next;
            }
            //生成IR
            //形参赋值
            int parmNum=regNum+1;
            tmp=astnode->left;
            while(tmp!=NULL){
                string sRegName="%"+std::to_string(parmNum+regNum);
                regNum++;
                Variable * sReg=new Variable(Type(tmp->type,tmp->arraysubs),sRegName,curFunc);
                Alloca * ir=new Alloca(
                    Type(tmp->type,tmp->arraysubs),sReg);
                    scope->addVar(astnode->svalue, sReg);
                curBblock->addIR(ir);
            }
            
            generateIR(astnode->right->left);
            generateIR(astnode->next);
                return NULL;
            }
            
            break;
        case COMPOUND_STMT:{
            //新建作用域
            Scope * tmpScope=new Scope();
            tmpScope->setPreScope(scope);
            scope=tmpScope;
            generateIR(astnode->left);
            generateIR(astnode->next);
            //退出时重置作用域
            scope=scope->getpre();
            return NULL;
        }
            break;
        case WHILE_STMT:{
            if(astnode->left->nodeType==BINARY_OPERATOR&&(astnode->left->ivalue==AND||astnode->left->ivalue==OR)){
                //新建条件基本块
                BasicBlock * conditionBblock=new BasicBlock("."+to_string(bblockNum++));
                //新建条件的结果基本块
                resultBblock=new BasicBlock("."+to_string(bblockNum++));
                //新建循环体基本块
                BasicBlock * bodyBblock=new BasicBlock("."+to_string(bblockNum++));
                //新建循环结束后语句基本块
                BasicBlock * nextBblock=new BasicBlock("."+to_string(bblockNum++));
                IRs->addBlock(curFunc->getname(), conditionBblock);
                
                breakBblock=nextBblock;
                continueBblock=conditionBblock;
                //设置最终的条件结果寄存器
                string RegName="%"+std::to_string(regNum++);
                Variable * sReg=new Variable(REG,RegName,curFunc);
                conditionPhi=new Phi(sReg);
                Jump * ir_jump=new Jump(conditionBblock);
                curBblock->addIR(ir_jump);

                curBblock=conditionBblock;
                generateIR_WhileCond(astnode->left);
                Jump * ir=new Jump(resultBblock);
                curBblock->addIR(ir);
                

                curBblock=resultBblock;
                curBblock->addIR(conditionPhi);
                Branch * ir_branch=new Branch(conditionPhi->getsReg(),bodyBblock,nextBblock);
                curBblock->addIR(ir_branch);

                curBblock=bodyBblock;
                Scope * tmpScope=new Scope();
                tmpScope->setPreScope(scope);
                scope=tmpScope;
                if(astnode->right->nodeType==COMPOUND_STMT)
                    generateIR(astnode->right->left);
                else
                    generateIR(astnode->right);
                Jump* ir_while=new Jump(conditionBblock);
                curBblock->addIR(ir_while);

                IRs->addBlock(curFunc->getname(), bodyBblock);
                IRs->addBlock(curFunc->getname(), resultBblock);
                IRs->addBlock(curFunc->getname(), nextBblock);

                curBblock=nextBblock;
                generateIR(astnode->next);
                return NULL;
            }
            else if(astnode->nodeType==BINARY_OPERATOR){
                //新建条件基本块
                BasicBlock * conditionBblock=new BasicBlock("."+to_string(bblockNum++));
                 //新建循环体基本块
                BasicBlock * bodyBblock=new BasicBlock("."+to_string(bblockNum++));
                //新建循环结束后语句基本块
                BasicBlock * nextBblock=new BasicBlock("."+to_string(bblockNum++));

                IRs->addBlock(curFunc->getname(), conditionBblock);
                Jump * ir_jump=new Jump(conditionBblock);
                curBblock->addIR(ir_jump);
                
                breakBblock=nextBblock;
                continueBblock=conditionBblock;
                curBblock=conditionBblock;
                Instruction * tmp=generateIR(astnode->left);
                BinaryOperation* cmp = dynamic_cast<BinaryOperation*>(tmp);
                Branch * ir_branch=new Branch(cmp->getsReg(),bodyBblock,nextBblock);
                curBblock->addIR(ir_branch);
                
                curBblock=bodyBblock;
                IRs->addBlock(curFunc->getname(), bodyBblock);
                Scope * tmpScope=new Scope();
                tmpScope->setPreScope(scope);
                scope=tmpScope;
                if(astnode->right->nodeType==COMPOUND_STMT)
                    generateIR(astnode->right->left);
                else
                    generateIR(astnode->right);
                Jump* ir_while=new Jump(conditionBblock);
                curBblock->addIR(ir_while);

                curBblock=nextBblock;
                IRs->addBlock(curFunc->getname(), nextBblock);
                generateIR(astnode->next);
                return NULL;
            }   
            else{
                Constant * result;
                if(astnode->left->ivalue!=0){
                    //新建循环体基本块
                    BasicBlock * bodyBblock=new BasicBlock("."+to_string(bblockNum++));
                    //新建循环结束后语句基本块
                    BasicBlock * nextBblock=new BasicBlock("."+to_string(bblockNum++));
                    
                    breakBblock=nextBblock;
                    continueBblock=bodyBblock;
                    
                    IRs->addBlock(curFunc->getname(), bodyBblock);
                    Jump * ir_jump=new Jump(bodyBblock);
                    curBblock->addIR(ir_jump);

                    curBblock=bodyBblock;
                    IRs->addBlock(curFunc->getname(), bodyBblock);
                    Scope * tmpScope=new Scope();
                    tmpScope->setPreScope(scope);
                    scope=tmpScope;
                    if(astnode->right->nodeType==COMPOUND_STMT)
                        generateIR(astnode->right->left);
                    else
                        generateIR(astnode->right);
                    Jump* ir_while=new Jump(bodyBblock);
                    curBblock->addIR(ir_while);
                    
                    generateIR(astnode->next);
                    return NULL;
                }
                else{generateIR(astnode->next);return NULL;}

            }
        }
            break;
        case IF_STMT://记得修剪优化语法树
        {
            BasicBlock * nextBblock=generateIR_IfStmt(astnode->right);
            BasicBlock * bodyBblock=generateIR_IfBody(astnode->left);
            BasicBlock * conditionBblock=generateIR_IfCond(astnode->if_cond);
            
            


        }
            

			break;
		case CONTINUE_STMT:{
            Jump * ir_jump=new Jump(continueBblock);
            curBblock->addIR(ir_jump);
            generateIR(astnode->next);
            return ir_jump;
        }
			break;
		case BREAK_STMT:{
            Jump * ir_jump=new Jump(breakBblock);
            curBblock->addIR(ir_jump);
            generateIR(astnode->next);
            return ir_jump;
        }

			break;
		case RETURN_STMT:

			break;
		case INTEGER_LITERAL:

			break;
		case FLOATING_LITERAL:

			break;
		case DECL_REF_EXPR:{//函数的DeclRefExpr在CallExpr中处理
            Scope * tmp=scope;
            Variable * var=tmp->lookup(astnode->svalue);
            while(var==NULL){
                tmp=scope->getpre();
                var=tmp->lookup(astnode->svalue);
            }
            //生成IR
            string tmpRegName="%"+std::to_string(regNum);
            regNum++;
            Variable *tReg=new Variable(var->getType(),tmpRegName,curFunc);
            Load * ir_load=new Load(tReg,var);
            curBblock->addIR(ir_load);
            return ir_load;
        }
			break;
		case BINARY_OPERATOR:


			break;
		case INIT_LIST_EXPR:

			break;
		case DECL_STMT:

			break;
		case VAR_DECL:{
            string varName;
            if(scope->isGlobal()){
                varName="@"+string(astnode->svalue);
            }
            else{
                varName="%"+std::to_string(regNum);
                regNum++;
            }
            
            Variable * varible=new Variable(Type(astnode->type,astnode->arraysubs),varName,curFunc);
            curFunc->addSymbols(varible);
            scope->addVar(astnode->svalue, varible);
            //生成IR
        }

			break;
		case UNARY_OPERATOR:

			break;
		case CALL_EXPR:

			break;
		case ARRAY_SUBSCRIPT_EXPR:

			break;
		case PAREN_EXPR:

			break;
		case NULL_STMT:

			break;
		default:
            break;
    };

}

Instruction * generateIR_WhileCond(past astnode){
    if(astnode==NULL)
        return NULL;
    switch(astnode->ivalue){
        case AND:{
            Instruction * left=NULL;
            //先处理左右节点的表达式
            if(astnode->left->ivalue==AND||astnode->left->ivalue==OR)
                left=generateIR_WhileCond(astnode->left);
            else
                left=generateIR(astnode->left);
            //翻译自身结构
            BinaryOperation* cmp = dynamic_cast<BinaryOperation*>(left);
            BasicBlock * rightBblock=new BasicBlock("."+to_string(bblockNum));
            Branch * ir_branch=new Branch(cmp->getsReg(),rightBblock,resultBblock);
            curBblock->addIR(ir_branch);
            curBblock=rightBblock;
            IRs->addBlock(curFunc->getname(), rightBblock);
            conditionPhi->addPhiSym(new Constant(I1,false), resultBblock);
            //后处理右节点表达式
            Instruction * right=generateIR(astnode->right);
            return ir_branch;        
        }
        case OR:{
            Instruction * left=NULL;
            //先处理左右节点的表达式
            if(astnode->left->ivalue==AND||astnode->left->ivalue==OR)
                left=generateIR_WhileCond(astnode->left);
            else
                left=generateIR(astnode->left);
            //翻译自身结构
            BinaryOperation* cmp = dynamic_cast<BinaryOperation*>(left);
            BasicBlock * rightBblock=new BasicBlock("."+to_string(bblockNum));
            Branch * ir_branch=new Branch(cmp->getsReg(),rightBblock,resultBblock);
            curBblock->addIR(ir_branch);
            curBblock=rightBblock;
            IRs->addBlock(curFunc->getname(), rightBblock);
            conditionPhi->addPhiSym(new Constant(I1,true), resultBblock);
            //后处理右节点表达式
            Instruction * right=generateIR(astnode->right);
            return ir_branch;
        } 
        default:
            return NULL;
    }
}

BasicBlock * generateIR_IfBody(past astnode,BasicBlock *nextBblock){
    BasicBlock * bodyBblock=new BasicBlock("."+to_string(bblockNum++));
    generateIR(astnode,bodyBblock);
    Jump * ir_jump=new Jump(nextBblock);
    bodyBblock->addIR(ir_jump);
    return bodyBblock;
}

BasicBlock * generateIR_IfCond(past astnode){
    if(astnode->nodeType==BINARY_OPERATOR&&(astnode->ivalue==OR||astnode->ivalue==AND)){
        BasicBlock * left=generateIR_IfCond(astnode->left);
        BasicBlock * right=generateIR_IfCond(astnode->right);
        Instruction * tmp=right->lastInstruction();
        BinaryOperation* cmp = dynamic_cast<BinaryOperation*>(tmp);
        if(astnode->ivalue==OR)
            Branch * ir_branch=new Branch(cmp->getsReg(),bodyBblock,nextBblock);

    }
    else if(astnode->nodeType==BINARY_OPERATOR){
        
    }
    else{

    }
}

BasicBlock * generateIR_IfStmt(past astnode){

}

