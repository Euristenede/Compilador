/*
 * Parser para uma calculadora avançada
 */
 
/* interface com o lexer */
extern int yylineno;
void yyerror(char *s, ...);

/* tabela de simbolos */
struct symbol { /* um nome de variável */
 char *name;
 double value;
 struct ast *func; /* stmt para função */
 struct symlist *syms; /* lista de argumentos */
};

/* tabela de simbolos de tamanho fixo */
#define NHASH 9997
struct symbol symtab[NHASH];

struct symbol *lookup(char*);

/* Lista de simbolos, para uma lista de argumentos */
struct symlist {
 struct symbol *sym;
 struct symlist *next;
};

struct symlist *newsymlist(struct symbol *sym, struct symlist *next);
void symlistfree(struct symlist *sl);

/* tipos de nós
 * + - * / |
 * 0-7 operadores de comparação, 04 igual, 02 menor que, 01 maior que
 * L expressão ou lista de comandos
 * I comando IF
 * W comando WHILE
 * N simbolo de referência
 * = atribuiao
 * S lista de simbolos
 * F chamada de função pre-definida
 * C chamada de função def. p/ usuario
 * O comando FOR
 */
 
enum bifs { /* funções pré-definidas */
 B_sqrt = 1,
 B_exp,
 B_log,
 B_print
};

/* nós na ast */
/* todos tem o nodetype inicial em comum */
struct ast {
 int nodetype;
 struct ast *l;
 struct ast *r;
};

struct fncall { /* funções pré-definidas */
 int nodetype; /* tipo F */
 struct ast *l;
 enum bifs functype;
};

struct ufncall { /* funções usuário */
 int nodetype; /* tipo C */
 struct ast *l; /* lista de arguments */
 struct symbol *s;
};

struct flow {
 int nodetype; /* tipo I ou W */
 struct ast *cond; /* condição */
 struct ast *tl; /* ramo "then" ou lista "do" */
 struct ast *el; /* ramo opcional "else" */
};

struct foor {
 int nodetype; /* tipo O */
 struct ast *init; /* inicialização */
 struct ast *cond; /* condição */
 struct ast *inc; /* incremento */
 struct ast *tl; /* ramo "then" ou lista "do" */
};

struct numval {
 int nodetype; /* tipo K */
 double number;
};

struct symref {
 int nodetype; /* tipo N */
 struct symbol *s;
};

struct symasgn {
 int nodetype; /* tipo = */
 struct symbol *s;
 struct ast *v; /* valor a ser atribuido */
};

/* construção de uma AST */
struct ast *newast(int nodetype, struct ast *l, struct ast *r);
struct ast *newcmp(int cmptype, struct ast *l, struct ast *r);
struct ast *newfunc(int functype, struct ast *l);
struct ast *newcall(struct symbol *s, struct ast *l);
struct ast *newref(struct symbol *s);
struct ast *newasgn(struct symbol *s, struct ast *v);
struct ast *newnum(double d);
struct ast *newflow(int nodetype, struct ast *cond, struct ast *tl, struct ast *tr);
struct ast *newfor(int nodetype, struct ast *init, struct ast *cond, struct ast *inc, struct ast *tl);

/* definição de uma função */
void dodef(struct symbol *name, struct symlist *syms, struct ast *stmts);

/* avaliação de uma AST */
double eval(struct ast *);

/* deletar e liberar uma AST */
void treefree(struct ast *);
