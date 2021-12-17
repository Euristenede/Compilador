/*
 * Parser para uma calculadora avançada
 */
 
%{
# include <stdio.h>
# include <stdlib.h>
# include "calculadora.h"
%}

%union {
	struct ast *a;
	double d;
	struct symbol *s;	/* qual o simbolo? */
	struct symlist *sl;
	int fn;				/* qual a função? */
}

/* declaração de tokens */
%token <d> NUMBER
%token <s> NAME
%token <fn> FUNC
%token EOL

%token IF THEN ELSE WHILE DO LET FOR /*Adicionando o comando FOR*/

%nonassoc <fn> CMP
%right '='
%left '+' '-'
%left '*' '/'
%nonassoc '|' UMINUS

%type <a> exp stmt list explist
%type <sl> symlist

%start calclist
%%

stmt: IF exp THEN list 			 			{ $$ = newflow('I', $2, $4, NULL); }
	| IF exp THEN list ELSE list 			{ $$ = newflow('I', $2, $4, $6); }
	| WHILE exp DO list 		 			{ $$ = newflow('W', $2, $4, NULL); }
	| FOR '(' exp ';' exp ';' exp ')' list	{ $$ = newfor('O', $3, $5, $7, $9); } /*Adicionando o comando FOR*/
	| exp
	;
	
exp: exp CMP exp { $$ = newcmp($2, $1, $3); }
   | exp '+' exp { $$ = newast('+', $1,$3); }
   | exp '-' exp { $$ = newast('-', $1,$3);}
   | exp '*' exp { $$ = newast('*', $1,$3); }
   | exp '/' exp { $$ = newast('/', $1,$3); }
   | '|' exp { $$ = newast('|', $2, NULL); }
   | '(' exp ')' { $$ = $2; }
   | '-' exp %prec UMINUS { $$ = newast('M', $2, NULL); }
   | NUMBER { $$ = newnum($1); }
   | NAME { $$ = newref($1); }
   | NAME '=' exp { $$ = newasgn($1, $3); }
   | FUNC '(' explist ')' { $$ = newfunc($1, $3); }
   | NAME '(' explist ')' { $$ = newcall($1, $3); }
   ;

list: {$$ = NULL} /*vazio*/
	| stmt ';' list { if ($3 == NULL) 
						$$ = $1;
					  else
						$$ = newast('L', $1, $3);
					}
	;

   
explist: exp
	   | exp ',' explist { $$ = newast('L', $1, $3); }
	   ;
		
symlist: NAME { $$ = newsymlist($1, NULL); }
	   | NAME ',' symlist { $$ = newsymlist($1, $3); }
	   ;

calclist: /* vazio */
		| calclist stmt EOL {
			printf("= %4.4g\n> ", eval($2));
			treefree($2);
		  }
		| calclist LET NAME '(' symlist ')' '=' list EOL {
			dodef($3, $5, $8);
			printf("Defined %s\n> ", $3->name); 
		  }
		| calclist error EOL { yyerror; printf("> "); }
		;
%%