/*
2022090914008-李锦辉-实验三
*/
%option yylineno
%option noyywrap

%{

#include"parser.tab.hh"
#include<string.h>
#include<stdio.h>
#include<string.h>
#include<map>
extern int dimension;
%}
letter_     [A-Za-z_]
digit       [0-9]
dedigit     [0-9]
ocdigit     [0-8]
hedigit     [0-9a-fA-F]
Ident       {letter_}({letter_}|{digit})*
deINT_VAL    [1-9]+[0-9]*
ocINT_VAL    0{ocdigit}*
heINT_VAL    0[Xx]{hedigit}*
INT_VAL     {deINT_VAL}|{ocINT_VAL}|{heINT_VAL}
deFLOAT_VAL   ({digit}*\.?{digit}+|{digit}+\.)([Ee][+-]?{digit}+)?
heFLOAT_VAL   0[Xx]({hedigit}*\.?{hedigit}+|{hedigit}+\.)([Pp][+-]?{hedigit}+)?
FLOAT_VAL   {deFLOAT_VAL}|{heFLOAT_VAL}
%%
"int"       {yylval.i=INT;return INT;}
"float"     {yylval.i=FLOAT;return FLOAT;}
"const"     {yylval.i=CONST;return CONST;}
"void"     {yylval.i=VOID;return VOID;}
"if"     {return IF;}
"else"     {return ELSE;}
"while"     {return WHILE;}
"break"     {return BREAK;}
"continue"     {return CONTINUE;}
"return"     {return RETURN;}


{Ident}     {yylval.s=(char *)malloc(sizeof(strlen(yytext)+1));
                strcpy(yylval.s,yytext);
                    return IDENT;}
{deINT_VAL}   {sscanf(yytext,"%d",&yylval.i);return INT_VAL;}
{ocINT_VAL}   {sscanf(yytext,"%o",&yylval.i);return INT_VAL;}
{heINT_VAL}   {sscanf(yytext,"%x",&yylval.i);return INT_VAL;}
{FLOAT_VAL} {sscanf(yytext,"%e",&yylval.f);return FLOAT_VAL;}
"//"[^\n]*\n {   ;}
"/*"([^*]|\*+[^/*])*"*/"   {   ;}
[ \t]   {   ;}
\n  {   ;}


"+"|"-"|"*"|"/"|"%"|"<"|">"|"|"|"!"|"="|"&"|"["|"]"|"{"|"}"|"("|")"|";"|"," {yylval.i=yytext[0];return yytext[0];}

"&&" {yylval.i=AND;return AND;}
"==" {yylval.i=EQUAL;return EQUAL;}
"!=" {yylval.i=NOT_EQUAL;return NOT_EQUAL;}
">=" {yylval.i=GRT_EQUAL;return GRT_EQUAL;}
"<<"    {yylval.i=LEFT_MOVE;return LEFT_MOVE;}
"<=" {yylval.i=LES_EQUAL;return LES_EQUAL;}
"||" {yylval.i=OR;return OR;}
<<EOF>> {return YYEOF;}

%%
