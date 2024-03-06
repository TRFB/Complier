//2022090914008-李锦辉-实验三
#include"ast.hpp"
#include"parser.tab.hh"
#include <cstddef>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<vector>
#include<iostream>
past newTransUnit(past compunit){
    past transunit=newAstNode();
	transunit->nodeType=TRANSLATION_UNIT;
    transunit->left=compunit;
	return transunit;
}

past newVarDecl(char * id,past val,int type,past arrsubscrip){
	past vardecl=newAstNode();
	vardecl->nodeType=VAR_DECL;
	vardecl->svalue=id;
	vardecl->left=arrsubscrip;
	vardecl->right=val;
	vardecl->type=type;
	return vardecl;
}

past newDeclStmt(past vardecl){
	past declstmt=newAstNode();
	declstmt->nodeType=DECL_STMT;
	declstmt->left=vardecl;
	return declstmt;
}

past newArrSubscr(past val,past nextarrsub){
	past arrsub=newAstNode();
	arrsub->nodeType=ARRAY_SUBSCRIPT_EXPR;
	arrsub->left=val;
	arrsub->right=nextarrsub;
	return arrsub;
}

past newInitListExpr(past val,past nextnode){
	past initlist=newAstNode();
	initlist->nodeType=INIT_LIST_EXPR;
	initlist->left=val;
	initlist->next=nextnode;
	return initlist;
}

past newFuncDef(int type,char *id,past params,past block){
	past funcdef=newAstNode();
	funcdef->nodeType=FUNCTION_DECL;
	funcdef->type=type;
	funcdef->svalue=id;
	funcdef->left=params;
	funcdef->right=block;
	return funcdef;
}

past newParmVarDecl(int type, past arrsubscrip,char * name){
	past parmvardecl=newAstNode();
	parmvardecl->nodeType=PARM_DECL;
	parmvardecl->type=type;
	parmvardecl->left=arrsubscrip;
	parmvardecl->svalue=name;
	return parmvardecl;
}

past newParmArray(void){
	past arraysub=newAstNode();
	arraysub->nodeType=ARRAY_SUBSCRIPT_EXPR;
	past zeroval=newAstNode();
	zeroval->nodeType=INTEGER_LITERAL;
	zeroval->ivalue=0;
	arraysub->left=zeroval;
	return arraysub;
}

past newCompoundStmt(past stmt){
	past comstmt=newAstNode();
	comstmt->nodeType=COMPOUND_STMT;
	comstmt->left=stmt;
	return comstmt;
}

past newBinaryOper(int oper,past left,past right){
	past var=newAstNode();
	var->nodeType=BINARY_OPERATOR;
	var->ivalue=oper;
	var->left=left;
	var->right=right;
	return var;
}

past newNullStmt(void){
	past nullstmt=newAstNode();
	nullstmt->nodeType=NULL_STMT;
	return nullstmt;
}

past newWhileStmt(past leftcondition,past rightstmt){
	past whilestmt=newAstNode();
	whilestmt->nodeType=WHILE_STMT;
	whilestmt->left=leftcondition;
	whilestmt->right=rightstmt;
	return whilestmt;
}

past newIfStmt(past condition,past leftstmt,past rightelse){
	past ifstmt=newAstNode();
	ifstmt->nodeType=IF_STMT;
	ifstmt->if_cond=condition;
	ifstmt->left=leftstmt;
	ifstmt->right=rightelse;
	return ifstmt;
}

past newBreakStmt(void){
	past breakstmt=newAstNode();
	breakstmt->nodeType=BREAK_STMT;
	return breakstmt;
}

past newContinueStmt(void){
	past continuestmt=newAstNode();
	continuestmt->nodeType=CONTINUE_STMT;
	return continuestmt;
}

past newReturnStmt(past val){
	past returnstmt=newAstNode();
	returnstmt->nodeType=RETURN_STMT;
	returnstmt->left=val;
	return returnstmt;
}

past newParenExpr(past exp){
	past parenexpr=newAstNode();
	parenexpr->nodeType=PAREN_EXPR;
	parenexpr->left=exp;
	return parenexpr;
}

past newIntLiteral(int val){
	past intliteral=newAstNode();
	intliteral->nodeType=INTEGER_LITERAL;
	intliteral->ivalue=val;
	return intliteral;
}

past newFloatLiteral(float val){
	past floatliteral=newAstNode();
	floatliteral->nodeType=FLOATING_LITERAL;
	floatliteral->fvalue=val;
	return floatliteral;
}

past newDeclRefExpr(char * s){
	past declref=newAstNode();
	declref->nodeType=DECL_REF_EXPR;
	declref->svalue=s;
	return declref;
}

past newCallExpr(past leftid,past rightparams){
	past callexpr=newAstNode();
	callexpr->nodeType=CALL_EXPR;
	callexpr->left=leftid;
	callexpr->right=rightparams;
	return callexpr;
}

past newUnaryExp(int oper,past left){
	past unaryexp=newAstNode();
	unaryexp->nodeType=UNARY_OPERATOR;
	unaryexp->ivalue=oper;
	unaryexp->left=left;
	return unaryexp;
}

past newAstNode(void)
{
	past node = (ast *)malloc(sizeof(ast));
	if(node == NULL)
	{
		printf("run out of memory.\n");
		exit(0);
	}
	memset(node, 0, sizeof(ast));
	return node;
}


void showAst(past node,int nest){
	if(node==NULL)
		return ;
	
	int i=0;
	for(i=1;i<nest;i++)
		printf("  ");
	if(nest!=0)
		printf("|-");
	switch(node->nodeType){
		case TRANSLATION_UNIT:
			printf("TranslationUnitDecl\n");
			break;
		case FUNCTION_DECL:
			
			printf("FunctionDecl %s ",node->svalue);
			printType(node);
			printf("\n");
			break;
		case PARM_DECL:
			printf("ParmVarDecl ");
			printType(node);
			printf(" %s",node->svalue);
			printf("\n");
			break;
		case COMPOUND_STMT:
			printf("CompoundStmt\n");
			break;
		case WHILE_STMT:
			printf("WhileStmt\n");
			break;
		case IF_STMT:
			printf("IfStmt\n");
			break;
		case CONTINUE_STMT:
			printf("ContinueStmt\n");
			break;
		case BREAK_STMT:
			printf("BreakStmt\n");
			break;
		case RETURN_STMT:
			printf("ReturnStmt\n");
			break;
		case INTEGER_LITERAL:
			printf("IntegerLiteral %d\n",node->ivalue);
			break;
		case FLOATING_LITERAL:
			printf("FloatingLiteral %f\n",node->fvalue);
			break;
		case DECL_REF_EXPR:
			printf("DeclRefExpr %s\n",node->svalue);
			break;
		case BINARY_OPERATOR:
			printf("BinaryOperator ");
			printBoper(node);
			break;
		case INIT_LIST_EXPR:
			printf("InitListExpr %zd\n",node->dimen);
			break;
		case DECL_STMT:
			printf("DeclStmt\n");
			break;
		case VAR_DECL:
			printf("VarDecl %s ",node->svalue);
			printType(node);
			printf("\n");
			break;
		case UNARY_OPERATOR:
			printf("UnaryOperator ");
			printUoper(node);
			break;
		case CALL_EXPR:
			printf("CallExpr \n");
			break;
		case ARRAY_SUBSCRIPT_EXPR:
			printf("ArraySubscriptExpr\n");
			break;
		case PAREN_EXPR:
			printf("ParenExpr\n");
			break;
		case NULL_STMT:
			printf("NullStmt\n");
			break;
		default:
			exit(EXIT_FAILURE);
			break;
	}
	if(node->if_cond!=NULL)
		showAst(node->if_cond,nest+1);
	showAst(node->left,nest+1);
	showAst(node->right,nest+1);
	showAst(node->next,nest);
}

void printUoper(past node){
	switch(node->ivalue){
		case '!':printf("!\n");break;
		case '+':printf("+\n");break;
		case '-':printf("-\n");break;
		default:
			exit(EXIT_FAILURE);
			break;
	}
}

void printBoper(past node){
	switch(node->ivalue){
		case '+':printf("+\n");break;
		case '-':printf("-\n");break;
		case '*':printf("*\n");break;
		case '/':printf("/\n");break;
		case '%':printf("%%\n");break;
		case '=':printf("=\n");break;
		case EQUAL:printf("==\n");break;
		case NOT_EQUAL:printf("!=\n");break;
		case '<':printf("<\n");break;
		case '>':printf(">\n");break;
		case LES_EQUAL:printf("<=\n");break;
		case GRT_EQUAL:printf(">=\n");break;
		case AND:printf("&&\n");break;
		case OR:printf("||\n");break;
		default:
			exit(EXIT_FAILURE);
			break;
	}
	return ;
}

void printType(past node){
	switch(node->type){
		case INT:printf("int");break;
		case VOID:printf("void");break;
		case FLOAT:printf("float");break;
		case const_float:printf("const float");break;
		case const_int:printf("const int");break;
		default: break;
	}
	return ;
}

past findTail(past node){
	while(node->next!=NULL){
		node=node->next;
	}
	return node;
}

past findRTail(past node){
	while(node->right!=NULL){
		node=node->right;
	}
	return node;
}

 void yyerror (char const *s) {
   fprintf (stderr, "%s\n", s);
 }

void editArray(past astnode,std::vector<int> &arraySubs,int curDimen,int dimension){
	static past prenode=NULL;
	if(astnode==NULL){
		return;
	}
	if(dimension==0){
		if(astnode->nodeType==INIT_LIST_EXPR){
			if(astnode->dimen!=0){
				prenode=astnode;
				editArray(astnode->left, arraySubs,astnode->dimen,0);
				editArray(astnode->next, arraySubs,astnode->dimen,0);
			}
			else{
				prenode=astnode;
				editArray(astnode->next, arraySubs,astnode->dimen,0);
			}
		}
		else if(astnode->nodeType==INTEGER_LITERAL||
			astnode->nodeType==FLOATING_LITERAL||
			astnode->nodeType==DECL_REF_EXPR||
			astnode->nodeType==ARRAY_SUBSCRIPT_EXPR)
		{
			past newILE=newInitListExpr(astnode, NULL);
			newILE->dimen=0;
			int arraysub=arraySubs[dimension];
			past tmp=astnode;
			past pretmp=tmp;
			while(arraysub!=0&&
					tmp!=NULL&&
				tmp->nodeType!=INIT_LIST_EXPR
				){
				pretmp=tmp;
				tmp=tmp->next;
				arraysub-=1;
			}
			pretmp->next=NULL;
			newILE->next=tmp;
			if(prenode->left==newILE->left)
				prenode->left=newILE;
			else
				prenode->next=newILE;
			prenode=newILE;
			editArray(tmp,arraySubs, curDimen,0);
		}	
 	}
	else{
		if(astnode->nodeType==INIT_LIST_EXPR){
			if(astnode->dimen>dimension){
				prenode=astnode;
				editArray(astnode->left, arraySubs,astnode->dimen,dimension);
				editArray(astnode->next, arraySubs,astnode->dimen,dimension);
			}
			else if(astnode->dimen==dimension){
				prenode=astnode;
				editArray(astnode->next, arraySubs,astnode->dimen,dimension);
			}
			else if(astnode->dimen==dimension-1)
			{
				past newILE=newInitListExpr(astnode, NULL);
				newILE->dimen=dimension;
				int arraysub=arraySubs[dimension];
				past tmp=astnode;
				past pretmp=tmp;
				while(arraysub!=0&&
						tmp!=NULL&&
					tmp->dimen!=dimension
					){
					pretmp=tmp;
					tmp=tmp->next;
					arraysub-=1;
				}
				pretmp->next=NULL;
				newILE->next=tmp;
				if(prenode->left==newILE->left)
					prenode->left=newILE;
				else
					prenode->next=newILE;
				prenode=newILE;
				editArray(tmp,arraySubs, curDimen,dimension);
			}
			else{
				;
			}
		}
 	}
  }

 void markArray(past astnode,std::vector<int>::size_type dimension){
	if(astnode==NULL)
		return;
	if(astnode->nodeType==INIT_LIST_EXPR)
		astnode->dimen=dimension-1;
	markArray(astnode->left, dimension-1);
	markArray(astnode->next, dimension);
 }