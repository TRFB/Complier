//2022090914008-李锦辉-实验三
%{
#include<iostream>
#include<stdio.h>
#include<stdlib.h>
#include"ast.hpp"
#include"symtable.hpp"
#include<map>
#include<string>
int yylex(void);
std::vector<int> arraySubs;
int dimension=0;
int arraysub;
int isarray=0;
extern char *yytext;
extern FILE * yyin;

extern int type;
extern past astroot;
%}




%union{
    
    struct _ast * astnode;
    int i;
    float f;
    char * s;
}

%token	<i> INT_VAL INT  FLOAT CONST VOID IF ELSE WHILE BREAK CONTINUE RETURN LES_EQUAL LEFT_MOVE GRT_EQUAL NOT_EQUAL EQUAL AND OR I1 REG
%token	<s> IDENT
%token <f> FLOAT_VAL
%token	<i> SITOFP FPTOSI BITCAST ZEXT TRUNC
%token	<i> ADD SUB MUL DIV MOD XOR
%token <i> ICMP CMP SLT SLE SGT SGE SNE SEQ
%token	<i> FCMP OLT OLE OGT OGE ONE OEQ
%token const_int const_float

%start TranslationUnit

%type <astnode> TranslationUnit CompUnit Decl FuncDef ConstDecl VarDecl ConstDecls VarDecls
%type <astnode> ConstDef VarDef InitVal InitVals ConstInitVal ConstInitVals ConstExp ConstExps
%type <astnode> Exp AddExp MulExp UnaryExp PrimaryExp LAndExp LOrExp EqExp RelExp
%type <astnode> FuncParams FuncParam ArraySubscripts Block BlockItem BlockItems CallParams
%type <astnode> Stmt LVal InFuncDecl
%type <i> Type 

%nonassoc LOWIF
%nonassoc ELSE
%%

TranslationUnit: {$$=NULL;}
    | CompUnit YYEOF{$$=newTransUnit($1);astroot=$$;showAst($$,0);}
    ;

CompUnit: Decl CompUnit {$1->next=$2;$$=$1;}
    | FuncDef CompUnit {past tail=findTail($1);tail->next=$2;$$=$1;}
    | Decl {$$=$1;}
    | FuncDef {$$=$1;}
    ;

Decl: ConstDecl{$$=$1;}
    | VarDecl{$$=$1;}
    ;

ConstDecl: CONST Type ConstDef ';' {$$=$3;}
    | CONST Type ConstDef ConstDecls ';' {$3->next=$4;$$=$3;}
    ;

ConstDecls: ',' ConstDef{$$=$2;}
    | ',' ConstDef ConstDecls{$2->next=$3;$$=$2;}
    ;

ConstDef: IDENT '=' ConstInitVal {if(type==INT)
                                        type=const_int;
                                    if(type==FLOAT)
                                        type=const_float;
                                    $$=newVarDecl($1,$3,type,NULL);}
    | IDENT ConstExps '=' ConstInitVal {if(type==INT)
                                        type=const_int;
                                    if(type==FLOAT)
                                        type=const_float;
                                    $$=newVarDecl($1,$4,type,$2);
                                    markArray($4,arraySubs.size());
                                    for(std::vector<int>::size_type i=0;
                                        i<arraySubs.size();i++)
                                        editArray($4,arraySubs,$4->dimen,i);
                                    ($$->arraysubs)=arraySubs;
                                    arraySubs.clear();
                                    
                                    }
    ;

ConstExps: '[' ConstExp ']' {$$=newArrSubscr($2,NULL);
                            arraySubs.push_back($2->ivalue);
                            isarray=1;
                            }
    | '[' ConstExp ']' ConstExps {$$=newArrSubscr($2,$4);
                                    arraySubs.push_back($2->ivalue);
                                    isarray=1;
                                    }
    ;

ConstInitVal: ConstExp {$$=$1; } 
    | '{' '}' {$$=newInitListExpr(NULL,NULL);}
    | '{' ConstInitVal '}' {$$=newInitListExpr($2,NULL);}
    | '{' ConstInitVal ConstInitVals '}' {$2->next=$3;$$=newInitListExpr($2,NULL);
                                            }
    ;

ConstInitVals: ',' ConstInitVal {$$=$2;}
    | ',' ConstInitVal ConstInitVals {$2->next=$3;$$=$2;}
    ;

VarDecl: Type VarDef ';'{$$=$2;}
    | Type VarDef VarDecls ';'{$2->next=$3;$$=$2;}
    ;

VarDecls: ',' VarDef {$$=$2;}
    |  ',' VarDef VarDecls  {$2->next=$3;$$=$2;}
    ;

VarDef: IDENT {$$=newVarDecl($1,NULL,type,NULL);}
    | IDENT '=' InitVal {$$=newVarDecl($1,$3,type,NULL);}
    | IDENT ConstExps {$$=newVarDecl($1,NULL,type,$2);}
    | IDENT ConstExps '=' InitVal{$$=newVarDecl($1,$4,type,$2);
                                    markArray($4,arraySubs.size());
                                    for(std::vector<int>::size_type i=0;
                                        i<arraySubs.size();i++)
                                        editArray($4,arraySubs,$4->dimen,i);
                                   ($$->arraysubs)=arraySubs;
                                   arraySubs.clear();
                                    }
    ;

InitVal: Exp {$$=$1;}
    | '{' '}' {$$=newInitListExpr(NULL,NULL);}
    | '{' InitVal '}' {$$=newInitListExpr($2,NULL);}
    | '{' InitVal InitVals '}' {$2->next=$3;$$=newInitListExpr($2,NULL);}
    ;

InitVals: ',' InitVal {$$=$2;}
    | ',' InitVal InitVals {$2->next=$3;$$=$2;}
    ;

FuncDef: Type IDENT '(' ')' Block {$$=newFuncDef($1,$2,NULL,$5);}
    | Type IDENT '(' FuncParams ')' Block {$$=newFuncDef($1,$2,$4,$6);}
    ;

FuncParams: FuncParam {$$=$1;}
    | FuncParams ',' FuncParam{past tail=findTail($1);
                                tail->next=$3;
                                $$=$1;}
    ;

FuncParam: Type IDENT {$$=newParmVarDecl($1,NULL,$2);}
    | Type IDENT '[' ']' {past arrsub=newParmArray();$$=newParmVarDecl($1,arrsub,$2);}
    | Type IDENT ArraySubscripts {$$=newParmVarDecl($1,$3,$2);
                                    $$->arraysubs=arraySubs;
                                    arraySubs.clear();}
    | Type IDENT '[' ']' ArraySubscripts {past arrsub=newParmArray();
                                       past righttail=findRTail($5);
                                       righttail->right=arrsub;
                                        $$=newParmVarDecl($1,$5,$2);
                                        $$->arraysubs=arraySubs;
                                        arraySubs.clear();}
    ;

Block: '{' BlockItems '}'{$$=newCompoundStmt($2);}
    | '{' '}'{$$=newCompoundStmt(NULL);}
    ;

BlockItems: BlockItem{$$=$1;}
    | BlockItem BlockItems{$1->next=$2;$$=$1;}
    ;

BlockItem: InFuncDecl{$$=$1;}
    | Stmt{$$=$1;}
    ;

InFuncDecl: ConstDecl{$$=newDeclStmt($1);}
    | VarDecl{$$=newDeclStmt($1);}
    ;

Stmt: LVal '=' Exp ';'{$$=newBinaryOper('=',$1,$3);}
    | ';'{$$=newNullStmt();}
    | Exp ';'{$$=$1;}
    | Block{$$=$1;}
    | WHILE '(' LOrExp ')' Stmt{$$=newWhileStmt($3,$5);}
    | IF '(' LOrExp ')' Stmt %prec LOWIF{$$=newIfStmt($3,$5,NULL);}
    | IF '(' LOrExp ')' Stmt  ELSE Stmt %prec ELSE{$$=newIfStmt($3,$5,$7);}
    | BREAK ';'{$$=newBreakStmt();}
    | CONTINUE ';'{$$=newContinueStmt();}
    | RETURN Exp ';'{$$=newReturnStmt($2);}
    | RETURN ';'{$$=newReturnStmt(NULL);}
    ;

Exp: AddExp{$$=$1;}
    ;

LVal: IDENT{$$=newDeclRefExpr($1);}
    | IDENT ArraySubscripts{past declref=newDeclRefExpr($1);
                            past righttail=findRTail($2);
                            righttail->right=declref;
                            $$=$2;
                            arraySubs.clear();
                            }
    ;

ArraySubscripts: '[' Exp ']'{$$=newArrSubscr($2,NULL);
                                arraySubs.push_back($2->ivalue);}
    | '[' Exp ']' ArraySubscripts{past arrsub=newArrSubscr($2,NULL);
                                    past righttail=findRTail($4);
                                            righttail->right=arrsub;
                                            $$=$4;
                                arraySubs.push_back($2->ivalue);}
    ;

PrimaryExp: '(' Exp ')' {$$=newParenExpr($2);}
    | LVal{$$=$1;}
    | INT_VAL {$$=newIntLiteral($1);}
    | FLOAT_VAL {$$=newFloatLiteral($1);}
    ;

UnaryExp: PrimaryExp {$$=$1;}
    | IDENT '(' ')' {past leftid=newDeclRefExpr($1);$$=newCallExpr(leftid,NULL);}
    | IDENT '(' CallParams ')' {past leftid=newDeclRefExpr($1);$$=newCallExpr(leftid,$3);}
    | '+' UnaryExp {$$=newUnaryExp('+',$2);}
    | '-' UnaryExp {$$=newUnaryExp('-',$2);}
    | '!' UnaryExp {$$=newUnaryExp('!',$2);}
    ;

CallParams: Exp {$$=$1;}
    | Exp ',' CallParams{$1->next=$3;$$=$1;}
    ;

MulExp: UnaryExp{$$=$1;}
    | MulExp '*' UnaryExp{$$=newBinaryOper('*',$1,$3);}
    | MulExp '/' UnaryExp{$$=newBinaryOper('/',$1,$3);}
    | MulExp '%' UnaryExp{$$=newBinaryOper('%',$1,$3);}
    ;

AddExp: MulExp{$$=$1;}
    | AddExp '+' MulExp{$$=newBinaryOper('+',$1,$3);}
    | AddExp '-' MulExp{$$=newBinaryOper('-',$1,$3);}
    ;

RelExp: AddExp{$$=$1;}
    | RelExp '<' AddExp{$$=newBinaryOper('<',$1,$3);}
    | RelExp '>' AddExp{$$=newBinaryOper('>',$1,$3);}
    | RelExp LES_EQUAL AddExp{$$=newBinaryOper($2,$1,$3);}
    | RelExp GRT_EQUAL AddExp{$$=newBinaryOper($2,$1,$3);}
    ;

EqExp: RelExp{$$=$1;}
    | EqExp EQUAL RelExp{$$=newBinaryOper($2,$1,$3);}
    | EqExp NOT_EQUAL RelExp{$$=newBinaryOper($2,$1,$3);}
    ;

LAndExp: EqExp{$$=$1;}
    | LAndExp AND EqExp  {$$=newBinaryOper($2,$1,$3);}
    ;

LOrExp: LAndExp{$$=$1;}
    | LOrExp OR LAndExp  {$$=newBinaryOper($2,$1,$3);}
    ;

ConstExp: AddExp {$$=$1;}
    ;

Type: INT{type=$1;$$=$1;}
    | FLOAT{type=$1;$$=$1;}
    | VOID{type=$1;$$=$1;}
    ;
%%