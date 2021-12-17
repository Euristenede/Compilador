/*
 * Lexer para uma calculadora avançada
 */

/* Reconhecimento de tokens para a calculadora */

%option noyywrap nodefault yylineno
%{
#include "calculadora.h"
#include "calculadora.tab.h"
%}

/*expoente float*/
EXP ([Ee][-+]?[0-9]+)

%%
"+"	|	/* operadores de caracter unico */
"-" |
"*" |
"/" |
"=" |
"|" |
"," |
";" |
"(" |
")" { return yytext[0]; }

">"	 { yylval.fn = 1; return CMP; }	/* operadores de comparação, todos são token CMP */
"<"	 { yylval.fn = 2; return CMP; }
"<>" { yylval.fn = 3; return CMP; }
"==" { yylval.fn = 4; return CMP; }
">=" { yylval.fn = 5; return CMP; }
"<=" { yylval.fn = 6; return CMP; }

"if"	{ return IF; }	/* palavras-chave */
"then"	{ return THEN; }
"else"	{ return ELSE; }
"while"	{ return WHILE; }
"do"	{ return DO; }
"let"	{ return LET; }
"for"	{ return FOR; } /*Adicionando o comando FOR*/


"sqrt"	{ yylval.fn = B_sqrt; return FUNC; } /* Funções pré-definidas */
"exp"	{ yylval.fn = B_exp; return FUNC; }
"log"	{ yylval.fn = B_log; return FUNC; }
"print"	{ yylval.fn = B_print; return FUNC; }

[a-zA-Z][a-zA-Z0-9]* { yylval.s = lookup(yytext); return NAME; } /*nomes*/
[0-9]+"."[0-9]*{EXP}? |
"."?[0-9]+{EXP}? { yylval.d = atof(yytext); return NUMBER; }

"//".*
[ /t] /* ignora espaço em branco */

\\\n { printf("c> "); } /* ignora continuação de linha */

\n { return EOL; }

. { yyerror("Caracter Desconhecido %c\n", *yytext); }
%%