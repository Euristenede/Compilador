/*
 * Funções auxiliares para uma calculadora avançada
 */
# include <stdio.h>
# include <stdlib.h>
# include <stdarg.h>
# include <string.h>
# include <math.h>
# include "calculadora.h"

/* Função em C para TS */
/* Função hash */
static unsigned symhash(char *sym)
{
	unsigned int hash = 0;
	unsigned c;
	
	while(c = *sym++) 
		hash = hash*9 ^ c;
	
	return hash;
}

struct symbol * lookup(char* sym){

 struct symbol *sp = &symtab[symhash(sym)%NHASH];
 int scount = NHASH; /* how many have we looked at */
 
 while(--scount >= 0) {
	if(sp->name && !strcmp(sp->name, sym)) 
	{ 
		return sp; 
	}
	if(!sp->name) { /* Nova entrada na TS */
		sp->name = strdup(sym);
		sp->value = 0;
		sp->func = NULL;
		sp->syms = NULL;
		return sp;
	}
	
	if(++sp >= symtab+NHASH) sp = symtab; /* tenta a próxima entrada */
 }
 yyerror("overflow na tabela de simbolos \n");
 abort(); /* tabela está cheia */
}

struct ast * newast(int nodetype, struct ast *l, struct ast *r)
{
	struct ast *a = malloc(sizeof(struct ast));

	if(!a) {
		yyerror("sem espaco");
		exit(0);
	}
	
	a->nodetype = nodetype;
	a->l = l;
	a->r = r;
	
	return a;
}

struct ast * newnum(double d){
	
	struct numval *a = malloc(sizeof(struct numval));

	if(!a) {
		yyerror("sem espaco");
		exit(0);
	}
	
	a->nodetype = 'K';
	a->number = d;
	
	return (struct ast *)a;
}

struct ast * newcmp(int cmptype, struct ast *l, struct ast *r){
	struct ast *a = malloc(sizeof(struct ast));

	if(!a) {
		yyerror("sem espaco");
		exit(0);
	}
	
	a->nodetype = '0' + cmptype;
	a->l = l;
	a->r = r;
	
	return a;
}

struct ast * newfunc(int functype, struct ast *l){
	struct fncall *a = malloc(sizeof(struct fncall));

	if(!a) {
		yyerror("sem espaco");
		exit(0);
	}
	
	a->nodetype = 'F';
	a->l = l;
	a->functype = functype;
	
	return (struct ast *)a;
}

struct ast * newcall(struct symbol *s, struct ast *l){
	struct ufncall *a = malloc(sizeof(struct ufncall));

	if(!a) {
		yyerror("sem espaco");
		exit(0);
	}
	
	a->nodetype = 'C';
	a->l = l;
	a->s = s;
	
	return (struct ast *)a;
}

struct ast * newref(struct symbol *s){
	struct symref *a = malloc(sizeof(struct symref));

	if(!a) {
		yyerror("sem espaco");
		exit(0);
	}
	
	a->nodetype = 'N';
	a->s = s;
	
	return (struct ast *)a;
}

struct ast * newasgn(struct symbol *s, struct ast *v){
	struct symasgn *a = malloc(sizeof(struct symasgn));
	
	if(!a) {
		yyerror("sem espaco");
		exit(0);
	}
	
	a->nodetype = '=';
	a->s = s;
	a->v = v;
	
	return (struct ast *)a;
}

struct ast * newflow(int nodetype, struct ast *cond, struct ast *tl, struct ast *el){
	struct flow *a = malloc(sizeof(struct flow));

	if(!a) {
		yyerror("sem espaco");
		exit(0);
	}
	
	a->nodetype = nodetype;
	a->cond = cond;
	a->tl = tl;
	a->el = el;
	
	return (struct ast *)a;
}

struct ast * newfor(int nodetype, struct ast *init, struct ast *cond, struct ast *inc, struct ast *tl){
	struct foor *a = malloc(sizeof(struct foor));

	if(!a) {
		yyerror("sem espaco");
		exit(0);
	}
	
	a->nodetype = nodetype;
	a->init = init;
	a->cond = cond;
	a->inc = inc;
	a->tl = tl;
	
	return (struct ast *)a;
}

/* Libera uma arvore da AST */
void treefree(struct ast *a){
	switch(a->nodetype) {
		/* duas subarvores */
		case '+':
		case '-':
		case '*':
		case '/':
		case '1': 
		case '2': 
		case '3': 
		case '4': 
		case '5': 
		case '6':
		case 'L':
			treefree(a->r);
		
		/* uma subarvore */
		case '|':
		case 'M': 
		case 'C': 
		case 'F':
			treefree(a->l);
		
		/* sem subarvore */
		case 'K': 
		case 'N':
			break;
		
		case '=':
			free( ((struct symasgn *)a)->v);
			break;
		
		/* acima de tres subarvores */
		case 'I': 
		case 'W':
			free( ((struct flow *)a)->cond);
			if( ((struct flow *)a)->tl) treefree( ((struct flow *)a)->tl);
			if( ((struct flow *)a)->el) treefree( ((struct flow *)a)->el);
			break;
		case 'O':
			free( ((struct foor *)a)->init);
			free( ((struct foor *)a)->cond);
			free( ((struct foor *)a)->inc);
			if( ((struct flow *)a)->tl) treefree( ((struct flow *)a)->tl);
			break;
	
		default: printf("Erro interno: free bad node %c\n", a->nodetype);
	}

	free(a); /* sempre libera o próprio nó */
}

struct symlist * newsymlist(struct symbol *sym, struct symlist *next){
	struct symlist *sl = malloc(sizeof(struct symlist));

	if(!sl) {
		yyerror("sem espaco");
		exit(0);
	}
	
	sl->sym = sym;
	sl->next = next;
	
	return sl;
}

/* libera uma lista de simbolos */
void symlistfree(struct symlist *sl) {
	struct symlist *nsl;
	
	while(sl) {
		nsl = sl->next;
		free(sl);
		sl = nsl;
	}
}

/*etapa principal >> avaliação de expressoes, comandos, funções, ...*/
static double callbuiltin(struct fncall *);
static double calluser(struct ufncall *);

double eval(struct ast *a)
{
	double v;
	if(!a) {
		yyerror("erro interno, null eval");
		return 0.0;
	}
	switch(a->nodetype) {
		/* Constante */
		case 'K': v = ((struct numval *)a)->number; break;
		
		/* referencia de nome */
		case 'N': v = ((struct symref *)a)->s->value; break;
		
		/* atribuição */
		case '=': v = ((struct symasgn *)a)->s->value = eval(((struct symasgn *)a)->v); break;
		
		/* expressoes */
		case '+': v = eval(a->l) + eval(a->r); break;
		case '-': v = eval(a->l) - eval(a->r); break;
		case '*': v = eval(a->l) * eval(a->r); break;
		case '/': v = eval(a->l) / eval(a->r); break;
		case '|': v = fabs(eval(a->l)); break;
		case 'M': v = -eval(a->l); break;
		
		/* Comparações */
		case '1': v = (eval(a->l) > eval(a->r))? 1 : 0; break;
		case '2': v = (eval(a->l) < eval(a->r))? 1 : 0; break;
		case '3': v = (eval(a->l) != eval(a->r))? 1 : 0; break;
		case '4': v = (eval(a->l) == eval(a->r))? 1 : 0; break;
		case '5': v = (eval(a->l) >= eval(a->r))? 1 : 0; break;
		case '6': v = (eval(a->l) <= eval(a->r))? 1 : 0; break;
		
		/* controle de fluxo */
		/* gramática permite expressoes vazias então deve ser verificadas */
		
		/* if/then/else */
		case 'I':
			if( eval( ((struct flow *)a)->cond) != 0) { /*verifica condição*/
				if( ((struct flow *)a)->tl) { /*ramo verdadeiro*/
					v = eval( ((struct flow *)a)->tl);
				} else
					v = 0.0; /* valor default */
			} else {
				if( ((struct flow *)a)->el) { /*ramo falso*/
					v = eval(((struct flow *)a)->el);
				} else
					v = 0.0; /* valor default */
			}
			break;
		
		/* while/do */
		case 'W':
			v = 0.0; /* valor default*/

			if( ((struct flow *)a)->tl) { /*testa se a lista de comandos não é vazia*/
				while( eval(((struct flow *)a)->cond) != 0) /*avalia a condição*/
					v = eval(((struct flow *)a)->tl); /*avalia comandos*/
			}
			break; /* valor do ultimo comando é valod do while/do */

		/* for */
		case 'O':
			v = 0.0; /* valor default*/

			if( ((struct foor *)a)->tl) { /*testa se a lista de comandos não é vazia*/
				for( eval((((struct foor *)a)->init));((((struct foor *)a)->cond) != 0); (((struct foor *)a)->inc)) /*avalia a condição*/
					v = eval(((struct foor *)a)->tl); /*avalia comandos*/
			}
			break; /* valor do ultimo comando é valor do for */

		/* lista de comandos */
		case 'L': eval(a->l); v = eval(a->r); break;
		
		case 'F': v = callbuiltin((struct fncall *)a); break;
		
		case 'C': v = calluser((struct ufncall *)a); break;
		
		default: printf("erro interno: bad node %c\n", a->nodetype);
	}
 return v;
}

static double callbuiltin(struct fncall *f) {
	enum bifs functype = f->functype;
	double v = eval(f->l);
	switch(functype) {
		case B_sqrt:
			return sqrt(v);
		case B_exp:
			return exp(v);
		case B_log:
			return log(v);
		case B_print:
			printf("= %4.4g\n", v);
			return v;
		default:
			yyerror("Funcao pre definida %d desconhecida ", functype);
			return 0.0;
	}
}

/* função definida pelo usuario */
void dodef(struct symbol *name, struct symlist *syms, struct ast *func) {
	if(name->syms) symlistfree(name->syms);
	if(name->func) treefree(name->func);
	name->syms = syms;
	name->func = func;
}

static double calluser(struct ufncall *f){
	struct symbol *fn = f->s; /* nome da função */
	struct symlist *sl; /* argumentos (originais) da função */
	struct ast *args = f->l; /* argumentos (usados) da função */
	double *oldval, *newval; /* salvar valores de argumentos */
	double v;
	int nargs;
	int i;
	
	if(!fn->func) {
		yyerror("chamada para funcao %s indefinida", fn->name);
		return 0;
	}
	
	/* contar argumentos */
	sl = fn->syms;
	for(nargs = 0; sl; sl = sl->next)
		nargs++;
	
	/* prepara para salvar os argumentos */
	oldval = (double *)malloc(nargs * sizeof(double));
	newval = (double *)malloc(nargs * sizeof(double));
	if(!oldval || !newval) {
		yyerror("Sem espaco em %s", fn->name); 
		return 0.0;
	}

	/* avaliação de argumentos */
	for(i = 0; i < nargs; i++) {
		if(!args) {
			yyerror("poucos argumentos na chamada da funcao %s", fn->name);
			free(oldval); 
			free(newval);
			return 0.0;
		}
		
		if(args->nodetype == 'L') { /* verifica se é uma lista de nós */
			newval[i] = eval(args->l);
			args = args->r;
		} else { /* verifica se é o final da lista */
			newval[i] = eval(args);
			args = NULL;
		}
	}

	/* salvar valores (originais) dos argumentos, atribuir novos valores */
	sl = fn->syms;
	for(i = 0; i < nargs; i++) {
		struct symbol *s = sl->sym;
		
		oldval[i] = s->value;
		s->value = newval[i];
		sl = sl->next;
	}
	
	free(newval);
	
	/* avanliação da função */
	v = eval(fn->func);
	
	/* recolocar os valores (originais) da funcao */
	sl = fn->syms;
	for(i = 0; i < nargs; i++) {
		struct symbol *s = sl->sym;
		s->value = oldval[i];
		sl = sl->next;
	}

	free(oldval);
	return v;
}

void yyerror(char *s, ...){
	va_list ap;
	va_start(ap, s);
	
	fprintf(stderr, "%d: error: ", yylineno);
	vfprintf(stderr, s, ap);
	fprintf(stderr, " \n");
}

int main(){
	printf("> ");
	return yyparse();
}
