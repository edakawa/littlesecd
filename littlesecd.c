// littlesecd.c -- Little SECD Machine
//
// This program made based on minilisp (nogc version) and micro scheme.
//
//     minilisp (nogc version):
//         https://github.com/rui314/minilisp/blob/nogc/minilisp.c
//     micro scheme:
//         http://www.nct9.ne.jp/m_hiroi/func/abcscm33.html
//
// And minilisp also has gc version.
//
//     minilisp:
//         https://github.com/rui314/minilisp
//
// I'd like to take this opportunity to thank you all.
//
// Author: Hajime Edakawa <hajime.edakawa@gmail.com>
// License: Public Domain

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

// -----------------------------------------------------------------------------
// 1. Lisp Objects
// -----------------------------------------------------------------------------

// The Lisp Object type
enum {
    TINT = 1,
    TCELL,
    TSYMBOL,
    TOPERATOR,
    TSPECIAL,
};

// Subtypes for TSPECIAL
enum {
    TNIL = 1,
    TTRUE,
    TDOT,
    TCPAREN,
    TCLOSURE,
};

struct Obj;

typedef void Op(struct Obj *s, struct Obj *e, struct Obj *c, struct Obj *d);

// The object type
typedef struct Obj {
    // The first word of the object represents the type of the object. Any code
    // that handles object needs to check its type first, then access the
    // following union members.
    int type;

    // Object values
    union {
	// Int
        int value;
	// Cell
        struct {
            struct Obj *car;
            struct Obj *cdr;
        };
	// Symbol
        char name[1];
	// SECD machine operator
        Op *op;
	// Subtype for special type
        int subtype;
    };
} Obj;

// Constants
static Obj *Nil;
static Obj *True;
static Obj *Dot;
static Obj *Cparen;
static Obj *Closure;

// The list containing all symbols. Such data structure is traditionally called
// the "obarray", but I avoid using it as a variable name as this is not an
// array but a list.
static Obj *Symbols;
static Obj *Globals;
static Obj *Opcodes;

// SECD Machine Operators
#define OP_LD     intern("ld")
#define OP_LDC    intern("ldc")
#define OP_LDG    intern("ldg")
#define OP_LDF    intern("ldf")
#define OP_ARGS   intern("args")
#define OP_APP    intern("app")
#define OP_RTN    intern("rtn")
#define OP_DUM    intern("dum")
#define OP_RAP    intern("rap")
#define OP_SEL    intern("sel")
#define OP_JOIN   intern("join")
#define OP_POP    intern("pop")
#define OP_DEF    intern("def")
#define OP_ADD    intern("add")
#define OP_SUB    intern("sub")
#define OP_MUL    intern("mul")
#define OP_DIV    intern("div")
#define OP_MOD    intern("mod")
#define OP_CONS   intern("cons")
#define OP_CAR    intern("car")
#define OP_CDR    intern("cdr")
#define OP_LT     intern("lt")
#define OP_LE     intern("le")
#define OP_NEQ    intern("neq")
#define OP_ATOM   intern("atom")
#define OP_EQ     intern("eq")
#define OP_STOP   intern("stop")

// Special Forms
#define SF_QUOTE  intern("quote")
#define SF_DEFINE intern("define")
#define SF_IF     intern("if")
#define SF_LAMBDA intern("lambda")
#define SF_LET    intern("let")
#define SF_LETREC intern("letrec")

// Primitive Functions
#define PRIM_CONS intern("cons")
#define PRIM_CAR  intern("car")
#define PRIM_CDR  intern("cdr")
#define PRIM_ADD  intern("+")
#define PRIM_SUB  intern("-")
#define PRIM_MUL  intern("*")
#define PRIM_DIV  intern("/")
#define PRIM_MOD  intern("%")
#define PRIM_LT   intern("<")
#define PRIM_LE   intern("<=")
#define PRIM_NEQ  intern("=")
#define PRIM_ATOM intern("atom")
#define PRIM_EQ   intern("eq")

//  Special Symbols
#define ATOM_T    intern("t")
#define ATOM_NIL  intern("nil")

// Miscellaneou
#define    CAR(x) ((Obj *)x)->car
#define    CDR(x) ((Obj *)x)->cdr
#define   CAAR(x) ((Obj *)x)->car->car
#define   CADR(x) ((Obj *)x)->cdr->car
#define   CDAR(x) ((Obj *)x)->car->cdr
#define   CDDR(x) ((Obj *)x)->cdr->cdr
#define  CADAR(x) ((Obj *)x)->car->cdr->car
#define  CADDR(x) ((Obj *)x)->cdr->cdr->car
#define  CDDDR(x) ((Obj *)x)->cdr->cdr->cdr
#define CADDAR(x) ((Obj *)x)->car->cdr->cdr->car
#define CADDDR(x) ((Obj *)x)->cdr->cdr->cdr->car

// -----------------------------------------------------------------------------
// 2. Constructors
// -----------------------------------------------------------------------------

static Obj *alloc(int type, size_t size) {
    size += offsetof(Obj, value);
    Obj *obj = malloc(size);
    obj->type = type;
    return obj;
}

static Obj *make_special(int subtype) {
    Obj *r = malloc(sizeof(void *) * 2);
    r->type = TSPECIAL;
    r->subtype = subtype;
    return r;
}

static Obj *make_symbol(char *name) {
    Obj *sym = alloc(TSYMBOL, strlen(name) + 1);
    strcpy(sym->name, name);
    return sym;
}

static Obj *make_operation(Op *op) {
    Obj *r = alloc(TOPERATOR, sizeof(Op *));
    r->op = op;
    return r;
}

static Obj *make_int(int value) {
    Obj *r = alloc(TINT, sizeof(int));
    r->value = value;
    return r;
}

static Obj *cons(Obj *car, Obj *cdr) {
    Obj *cell = alloc(TCELL, sizeof(Obj *) * 2);
    cell->car = car;
    cell->cdr = cdr;
    return cell;
}

// ((x . y) . a)
static Obj *acons(Obj *x, Obj *y, Obj *a) {
    return cons(cons(x, y), a);
}

// -----------------------------------------------------------------------------
// 3. Parser
// -----------------------------------------------------------------------------

static Obj *read(void);

static void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(1);
}

static int peek(void) {
    int c = getchar();
    ungetc(c, stdin);
    return c;
}

static Obj *intern(char *name) {
    for (Obj *p = Symbols; p != Nil; p = p->cdr)
        if (strcmp(name, p->car->name) == 0)
            return p->car;
    Obj *sym = make_symbol(name);
    Symbols = cons(sym, Symbols);
    return sym;
}

static Obj *read_quote(void) {
    return cons(SF_QUOTE, cons(read(), Nil));
}

static int read_number(int val) {
    while (isdigit(peek()))
        val = val * 10 + (getchar() - '0');
    return val;
}

#define SYMBOL_MAX_LEN 200

static Obj *read_symbol(char c) {
    char buf[SYMBOL_MAX_LEN + 1];
    int len = 1;
    buf[0] = c;
    while (isalnum(peek()) || strchr("~!@#$%^&*-_=+:/?<>", peek())) {
        if (SYMBOL_MAX_LEN <= len)
            error("Symbol name too long");
        buf[len++] = getchar();
    }
    buf[len] = '\0';
    return intern(buf);
}

static void skip_line(void) {
    for (;;) {
        int c = getchar();
        if (c == EOF || c == '\n')
            return;
        if (c == '\r') {
            if (peek() == '\n')
                getchar();
            return;
        }
    }
}

static Obj *read_list(void) {
    Obj *obj = read();
    if (!obj)
        error("Unclosed parenthesis");
    if (obj == Dot)
        error("Stray dot");
    if (obj == Cparen)
        return Nil;
    Obj *head, *tail;
    head = tail = cons(obj, Nil);

    for (;;) {
        Obj *obj = read();
        if (!obj)
            error("Unclosed parenthesis");
        if (obj == Cparen)
            return head;
        if (obj == Dot) {
            tail->cdr = read();
            if (read() != Cparen)
                error("Closed parenthesis expected after dot");
            return head;
        }
        tail->cdr = cons(obj, Nil);
        tail = tail->cdr;
    }
}

static Obj *read(void) {
    for (;;) {
        int c = getchar();
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
            continue;
        if (c == EOF)
            return NULL;
        if (c == ';') {
            skip_line();
	    continue;
        }
        if (c == '(')
            return read_list();
        if (c == ')')
            return Cparen;
        if (c == '.')
            return Dot;
        if (c == '\'')
	    return read_quote();
        if (isdigit(c))
            return make_int(read_number(c - '0'));
        if (c == '-' && isdigit(peek()))
            return make_int(-read_number(0));
        if (isalpha(c) || strchr("~!@#$%^&*-_=+:/?<>", c))
            return read_symbol(c);
        error("Don't know how to handle %c", c);
    }
}

static void print(Obj *obj) {
    switch (obj->type) {
    case TINT:
        printf("%d", obj->value);
        return;
    case TCELL:
	if (CAR(obj)->type == TSPECIAL && CAR(obj) == Closure) {
	    printf("<closure>");
	    return;
	}
        printf("(");
        for (;;) {
	    print(CAR(obj));
            if (CDR(obj) == Nil)
                break;
            if (CDR(obj)->type != TCELL || (CADR(obj)->type == TSPECIAL &&
	        CADR(obj) == Closure)) {
                printf(" . ");
		if (CDR(obj)->type != TCELL)
		    print(obj->cdr);
		else
		    printf("<closure>");
		break;
            }
            printf(" ");
            obj = obj->cdr;
        }
        printf(")");
        return;
    case TSYMBOL:
        printf("%s", obj->name);
        return;
    case TOPERATOR:
	printf("<operator>");
	return;
    case TSPECIAL:
        if (obj == Nil)
            printf("nil");
        else if (obj == True)
            printf("t");
        else
            error("Bug: print: Unknown subtype: %d", obj->subtype);
        return;
    default:
        error("Bug: print: Unknown tag type: %d", obj->type);
    }
}

static int list_length(Obj *list) {
    int len = 0;
    for (;;) {
	if (list == Nil)
	    return len;
	if (list->type != TCELL)
	    error("list_length: cannot handle dotted list");
	list = CDR(list);
	len++;
    }
}

// -----------------------------------------------------------------------------
// 4. VM
// -----------------------------------------------------------------------------

static Obj *fetch(Obj *sym) {
    for (Obj *p = Opcodes; p != Nil; p = CDR(p)) {
	Obj *bind = CAR(p);
	if (sym == CAR(bind))
	    return bind;
    }
    error("fetch: Unknown operator '%s'", sym->name);
    // NOTREACHED
    return Nil;
}

static Obj *pop(Obj **obj) {
    Obj *r = CAR(*obj);
    *obj = CDR(*obj);
    return r;
}

static void vm(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *sym = pop(&c);
    Obj *bind = fetch(sym);
    CDR(bind)->op(s, e, c, d);
}

static Obj *drop(Obj *ls, int n) {
    return (n == 0) ? ls : drop(CDR(ls), n - 1);
}

static Obj *list_ref(Obj *e, int n) {
    for (int i = 0; i < n; i++) {
        if (e->type != TCELL)
            error("argument out of range");
        e = CDR(e);
    }
    if (e->type != TCELL)
        error("argument out of range");
    return CAR(e);
}

static Obj *get_lvar(Obj *e, int i, int j) {
    e = list_ref(e, i);
    return (0 <= j) ? list_ref(e, j) : drop(e, -(j + 1));
}

// S E (LD (i . j) . C) D => (v . S) E C D
// v: (list-ref (list-ref E i) j)
static void op_ld(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *i = CAAR(c);                            // i
    Obj *j = CDAR(c);                            // j
    Obj *v = get_lvar(e, i->value, j->value);    // v
    Obj *code = CDR(c);                          // C
    Obj *stack = cons(v, s);                     // (v . S)
    vm(stack, e, code, d);
}

// S E (LDC constant . C) D => (constant . S) E C D
static void op_ldc(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *constant = CAR(c);            // constant
    Obj *code = CDR(c);                // C
    Obj *stack = cons(constant, s);    // (constant . S)
    vm(stack, e, code, d);
}

// S E (ldg sym . C) D => (v . S) E C D
// v: global value of sym
static void op_ldg(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *sym = CAR(c);                          // sym
    for (Obj *cell = Globals; cell != Nil; cell = CDR(cell)) {
	Obj *bind = CAR(cell);
	if (sym == CAR(bind)) {
	    Obj *code = CDR(c);                 // C
	    Obj *stack = cons(CDR(bind), s);    // (v . S)
	    vm(stack, e, code, d);
	    return;
	}
    }
    error("Undefined symbol: %s", sym->name);
}

// S E (ldf C' . C) D => ((<closure> C' E) . S) E C D
static void op_ldf(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *closure = cons(Closure, cons(CAR(c), cons(e, Nil)));
    Obj *code = CDR(c);               // C
    Obj *stack = cons(closure, s);    // ((<closure> C' E) . S)
    vm(stack, e, code, d);
}

// (v1 ... vN . S) E (args n . C) D => (vs . S) E C D
// vs: (list v1 ... vN)
static void op_args(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *n = CAR(c);             // n
    Obj *vs = Nil;
    for (int i = 0; i < n->value; i++) {
	Obj *obj = CAR(s);       // v1
	s = CDR(s);              // (v2 ... vN . S)
	vs = cons(obj, vs);      // (list v1 ... vN)
    }
    Obj *code = CDR(c);          // C
    Obj *stack = cons(vs, s);    // (vs . S)
    vm(stack, e, code, d);
}

// ((<closure> C' E') v . S) E (app . C) D => Nil (v . E') C' (S E C . D)
static void op_app(Obj *s, Obj *e, Obj *c, Obj *d) {
    if (CAR(s)->type != TCELL || CAAR(s)->type != TSPECIAL ||
	CAAR(s)->subtype != TCLOSURE)
	error("The head of a list must be a function");
    Obj *cp = CADAR(s);                                // C'
    Obj *dump = cons(CDDR(s), cons(e, cons(c, d)));    // (S E C . D)
    Obj *env = cons(CADR(s), CADDAR(s));               // (v . E')
    Obj *stack = Nil;                                  // Nil
    vm(stack, env, cp, dump);
}

// (v . S) E (rtn . C) (S' E' C' . D) => (v . S') E' C' D
static void op_rtn(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *ep = CADR(d);                    // E'
    Obj *cp = CADDR(d);                   // C'
    Obj *dump = CDDDR(d);                 // D
    Obj *stack = cons(CAR(s), CAR(d));    // (v . S')
    vm(stack, ep, cp, dump);
}

// S E (dum . C) D => S (O . E) C D
static void op_dum(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *env = cons(Nil, e);    // (O . E)
    vm(s, env, c, d);
}

// ((<closure> C' E') v . S) (O . E) (rap . C) D =>
//     Nil rplaca(E', v) C' (S E C . D)
static void op_rap(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *cp = CADAR(s);                                     // C'
    Obj *ep = CADDAR(s);                                    // E'
    Obj *v = CADR(s);                                       // v
    Obj *dump = cons(CDDR(s), cons(CDR(e), cons(c, d)));    // (S E C . D)
    Obj *stack = Nil;                                       // Nil
    CAR(ep) = v;                                            // rplaca(E', v)
    vm(stack, ep, cp, dump);
}

// (v . S) E (sel ct cf . C) D => S E ct (C . D)
//                             => S E cf (C . D)
static void op_sel(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *v = CAR(s);                 // v
    Obj *ct = CAR(c);                // ct
    Obj *cf = CADR(c);               // cf
    Obj *dump = cons(CDDR(c), d);    // (C . D)
    Obj *stack = CDR(s);             // S
    if (v != Nil)
	vm(stack, e, ct, dump);
    else
	vm(stack, e, cf, dump);
}

// S E (join . ()) (C . D) => S E C D
static void op_join(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *dump = CDR(d);    // D
    Obj *code = CAR(d);    // C
    vm(s, e, code, dump);
}

// (v . S) E (pop . C) D => S E C D
static void op_pop(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *stack = CDR(s);    // S
    vm(stack, e, c, d);
}

// (v . S) E (def sym . C) D => (sym . S) E C D
// v: global value of sym
static void op_def(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *v = CAR(s);                   // v
    Obj *sym = CAR(c);                 // sym
    Obj *code = CDR(c);                // C
    Obj *stack = cons(sym, CDR(s));    // (sym . S)
    Globals = acons(sym, v, Globals);
    vm(stack, e, code, d);
}

// (x y . S) E (add . C) D => ((y + x) . S) E C D
static void op_add(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *x = CAR(s);                             // x
    Obj *y = CADR(s);                            // y
    if (x->type != TINT || y->type != TINT)
	error("+ takes only numbers");
    Obj *sum = make_int(y->value + x->value);
    Obj *stack = cons(sum, CDDR(s));             // ((y + x) . S)
    vm(stack, e, c, d);
}

// (x y . S) E (sub . C) D => ((y - x) . S) E C D
static void op_sub(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *x = CAR(s);                               // x
    Obj *y = CADR(s);                              // y
    if (x->type != TINT || y->type != TINT)
	error("- takes only numbers");
    Obj *diff = make_int(y->value - x->value);
    Obj *stack = cons(diff, CDDR(s));              // ((y - x) . S)
    vm(stack, e, c, d);
}

// (x y . S) E (mul . C) D => ((y * x) . S) E C D
static void op_mul(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *x = CAR(s);                               // x
    Obj *y = CADR(s);                              // y
    if (x->type != TINT || y->type != TINT)
	error("* takes only numbers");
    Obj *prod = make_int(y->value * x->value);
    Obj *stack = cons(prod, CDDR(s));              // ((y * x) . S)
    vm(stack, e, c, d);
}

// (x y . S) E (div . C) D => ((y / x) . S) E C D
static void op_div(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *x = CAR(s);                               // x
    Obj *y = CADR(s);                              // y
    if (x->type != TINT || y->type != TINT)
	error("/ takes only numbers");
    if (x->value == 0)
        error("division by zero");
    Obj *quot = make_int(y->value / x->value);
    Obj *stack = cons(quot, CDDR(s));              // ((y / x) . S)
    vm(stack, e, c, d);
}

// (x y . S) E (mod . C) D => ((y % x) . S) E C D
static void op_mod(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *x = CAR(s);                               // x
    Obj *y = CADR(s);                              // y
    if (x->type != TINT || y->type != TINT)
	error("% takes only numbers");
    if (x->value == 0)
        error("division by zero");
    Obj *quot;
    if ((x->value < 0 && y->value > 0) || (x->value > 0 && y->value < 0)) {
        quot = make_int(y->value % x->value);
        quot->value += x->value;
    } else if (y->value == 0)
        quot = make_int(0);
    else
        quot = make_int(y->value % x->value);
    Obj *stack = cons(quot, CDDR(s));              // ((y / x) . S)
    vm(stack, e, c, d);
}

// (x y . S) E (cons . C) D => ((y . x) . S) E C D
static void op_cons(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *x = CAR(s);                           // x
    Obj *y = CADR(s);                          // y
    Obj *stack = cons(cons(y, x), CDDR(s));    // ((y . x) . S)
    vm(stack, e, c, d);
}

// ((x . y) . S) E (car . C) D => (x . S) E C D
static void op_car(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *cell = CAR(s);
    if (cell->type != TCELL)
	error("Malformed car");
    Obj *x = CAR(cell);              // x
    Obj *stack = cons(x, CDR(s));    // (x . S)
    vm(stack, e, c, d);
}

// ((x . y) . S) E (cdr . C) D => (y . S) E C D
static void op_cdr(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *cell = CAR(s);
    if (cell->type != TCELL)
	error("Malformed cdr");
    Obj *y = CDR(cell);              // y
    Obj *stack = cons(y, CDR(s));    // (y . S)
    vm(stack, e, c, d);
}

// (x y S) . E (lt . C) D => (y<x . S) E C D
static void op_lt(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *x = CAR(s);                                // x
    Obj *y = CADR(s);                               // y
    if (x->type != TINT || y->type != TINT)
	error("< takes only numbers");
    Obj *v = (y->value < x->value) ? True : Nil;    // y<x
    Obj *stack = cons(v, CDDR(s));                  // (y<x . S)
    vm(stack, e, c, d);
}

// (x y S) . E (le . C) D => (y<=x . S) E C D
static void op_le(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *x = CAR(s);                                // x
    Obj *y = CADR(s);                               // y
    if (x->type != TINT || y->type != TINT)
	error("< takes only numbers");
    Obj *v = (y->value <= x->value) ? True : Nil;   // y<=x
    Obj *stack = cons(v, CDDR(s));                  // (y<=x . S)
    vm(stack, e, c, d);
}

// (x y S) . E (neq . C) D => (y=x . S) E C D
static void op_neq(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *x = CAR(s);                                // x
    Obj *y = CADR(s);                               // y
    if (x->type != TINT || y->type != TINT)
	error("= takes only numbers");
    Obj *v = (y->value == x->value) ? True : Nil;   // y=x
    Obj *stack = cons(v, CDDR(s));                  // (y=x . S)
    vm(stack, e, c, d);
}

// (a . S) E (atom . C) D => (t . S) E C D
static void op_atom(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *a = CAR(s);                                                  // a
    Obj *t = (a->type == TINT || a->type == TSYMBOL) ? True : Nil;    // t
    Obj *stack = cons(t, CDR(s));                                     // (t . S)
    vm(stack, e, c, d);
}

// (a b . S) E (eq . C) D => (t . S) E C D
static void op_eq(Obj *s, Obj *e, Obj *c, Obj *d) {
    Obj *a = CAR(s);                                             // a
    Obj *b = CADR(s);                                            // b
    int c1 = (a->type == TINT || a->type == TSYMBOL) ? 1 : 0;
    int c2 = (b->type == TINT || b->type == TSYMBOL) ? 1 : 0;
    Obj *t;
    if (c1 && c2) {
        if (a->type == TINT && b->type == TINT)
            t = (a->value == b->value) ? True : Nil;
        else
            t = (a == b) ? True : Nil;
    } else
        t = Nil;
    Obj *stack = cons(t, CDDR(s));                               // (t . S)
    vm(stack, e, c, d);
}

// (v . S) E (stop . C) D => print v then stop secd machine
static void op_stop(Obj *s, Obj *e, Obj *c, Obj *d) {
    print(CAR(s));
    putchar('\n');
}

// -----------------------------------------------------------------------------
// 6. Compiler
// -----------------------------------------------------------------------------

static Obj *position_var(Obj *sym, Obj *ls) {
    int i = 0;
    for (;;) {
	if (ls == Nil)
	    return Nil;
	if (ls->type == TSYMBOL)
	    return (ls == sym) ? make_int(-(i + 1)) : Nil;
	if (sym == CAR(ls))
	    return make_int(i);
	i++;
	ls = CDR(ls);
    }
}

static Obj *location(Obj *sym, Obj *ls) {
    int i = 0;
    for (;;) {
	if (ls == Nil)
	    return Nil;
	Obj *j = position_var(sym, CAR(ls));
	if (j->type == TINT)
	    return cons(make_int(i), j);
	i++;
	ls = CDR(ls);
    }
}

static Obj *length(Obj *expr) {
    int i = 0;
    for (Obj *obj = expr; obj != Nil; obj = CDR(obj))
	i++;
    return make_int(i);
}

static Obj *comp(Obj *expr, Obj *env, Obj *code);

static Obj *term_location(Obj *term, Obj *env, Obj *code) {
    Obj *pos = location(term, env);
    return (pos == Nil) ? comp(term, env, code) : cons(OP_LD, cons(pos, code));
}

static Obj *symbol_location(Obj *sym, Obj *env, Obj *code) {
    Obj *pos = location(sym, env);
    return (pos == Nil) ? cons(OP_LDG, cons(sym, code)) :
        cons(OP_LD, cons(pos, code));
}

static Obj *complis(Obj *expr, Obj *env, Obj *code) {
    if (expr == Nil)
	return code;
    else
	return comp(CAR(expr), env, complis(CDR(expr), env, code));
}

static Obj *comp_body(Obj *body, Obj *env, Obj *code) {
    if (CDR(body) == Nil)
	return comp(CAR(body), env, code);
    else
	return comp(CAR(body),
		    env,
		    cons(OP_POP, comp_body(CDR(body), env, code)));
}

static Obj *comp(Obj *expr, Obj *env, Obj *code) {
    int len;
    Obj *head;
    switch (expr->type) {
    case TINT:
	return cons(OP_LDC, cons(expr, code));
    case TSYMBOL:
	return symbol_location(expr, env, code);
    case TCELL:
	len = list_length(expr);
	head = CAR(expr);
	if (SF_QUOTE == head) {
	    // 'expr
	    if (len != 2)
		error("Malformed quote");
	    return cons(OP_LDC, cons(CADR(expr), code));
	}
	if (SF_DEFINE == head) {
	    // (define sym expr)
	    if (len != 3 || CADR(expr)->type != TSYMBOL)
		error("Malformed define");
	    Obj *sym = CADR(expr);
	    Obj *val = CADDR(expr);
	    return comp(val, env, cons(OP_DEF, cons(sym, code)));
	}
	if (SF_IF == head) {
	    // (if expr t-clause f-clause)
	    if (len < 3 || len > 4)
		error("Malformed if");
	    Obj *t_clause, *f_clause;
	    t_clause = comp(CADDR(expr), env, cons(OP_JOIN, Nil));
	    if (CDDDR(expr) == Nil)
		f_clause = cons(OP_LDG, cons(ATOM_NIL, cons(OP_JOIN, Nil)));
	    else
		f_clause = comp(CADDDR(expr), env, cons(OP_JOIN, Nil));
	    return comp(CADR(expr), env, cons(OP_SEL, cons(t_clause,
	        cons(f_clause, code))));
	}
	if (SF_LAMBDA == head) {
	    // (lambda (vars) body ...)
	    if (len < 3 || CDDR(expr)->type != TCELL)
		error("Malformed lambda");
	    for (Obj *p = CADR(expr); p->type == TCELL; p = CDR(p))
		if (CAR(p)->type != TSYMBOL)
		    error("lambda: Parameter must be a symbol");
	    Obj *body = CDDR(expr);               // body
	    Obj *vars = cons(CADR(expr), env);    // vars
	    Obj *t_code = cons(OP_RTN, Nil);      // (rtn . Nil)
	    return cons(OP_LDF, cons(comp_body(body, vars, t_code), code));
	}
	if (SF_LET == head) {
	    // (let body (var1 . expr1) (var2 . expr2) ...)
	    if (len == 1)
		error("Malformed let");
	    if (len == 2)
		return cons(OP_LDG, cons(ATOM_NIL, code));
	    Obj *body = CADR(expr);
	    Obj *bind = CDDR(expr);
	    Obj *vars = Nil;
	    Obj *exprs = Nil;
	    for (Obj *obj = bind; obj != Nil; obj = CDR(obj)) {
		Obj *v = CAAR(obj);
		Obj *e = CDAR(obj);
		if (v->type != TSYMBOL)
		    error("let: Parameter must be a symbol");
		vars = cons(v, vars);
		exprs = cons(e, exprs);
	    }
	    Obj *lambda = cons(cons(SF_LAMBDA, cons(vars, cons(body, Nil))),
	        exprs);
	    return comp(lambda, env, code);
	}
	if (SF_LETREC == head) {
	    // (letrec body (var1 . expr1) (var2 . expr2) ...)
	    if (len == 1)
		error("Malformed letrec");
	    if (len == 2)
		return cons(OP_LDG, cons(ATOM_NIL, code));
	    Obj *body = CADR(expr);
	    Obj *bind = CDDR(expr);
	    Obj *vars = Nil;
	    Obj *exprs = Nil;
	    for (Obj *obj = bind; obj != Nil; obj = CDR(obj)) {
		Obj *v = CAAR(obj);
		Obj *e = CDAR(obj);
		if (v->type != TSYMBOL)
		    error("letrec: Parameter must be a symbol");
		vars = cons(v, vars);
		exprs = cons(e, exprs);
	    }
	    return cons(OP_DUM,
			complis(exprs,
				cons(vars, env),
				cons(OP_ARGS,
				     cons(length(exprs),
					  cons(OP_LDF,
					       cons(comp(body,
							 cons(vars, env),
							 cons(OP_RTN, Nil)),
						    cons(OP_RAP, code)))))));
	}
	if (PRIM_CONS == head) {
	    // (cons expr1 expr2)
	    if (len != 3)
		error("Malformed cons");
	    Obj *expr1 = CADR(expr);
	    Obj *expr2 = CADDR(expr);
	    return comp(expr1, env, comp(expr2, env, cons(OP_CONS, code)));
	}
	if (PRIM_CAR == head) {
	    // (car <cell>)
	    if (len != 2)
		error("Malformed car");
	    return comp(CADR(expr), env, cons(OP_CAR, code));
	}
	if (PRIM_CDR == head) {
	    // (cdr <cell>)
	    if (len != 2)
		error("Malformed cdr");
	    return comp(CADR(expr), env, cons(OP_CDR, code));
	}
	if (PRIM_ADD == head) {
	    // (+ x y z ...)
	    if (len == 1) {
		Obj *id = make_int(0);
		return cons(OP_LDC, cons(id, code));
	    }
	    Obj *nums = CDR(expr);
	    if (len == 2) {
		Obj *term = CAR(nums);
		if (term->type == TINT)
		    return cons(OP_LDC, cons(term, code));
		return term_location(term, env, code);
	    }
	    if (len >= 3) {
		Obj *x = CAR(nums);
		Obj *y = CADR(nums);
		Obj *tail = cons(OP_ADD, Nil);
		Obj *ret = term_location(y, env, tail);
		ret = term_location(x, env, ret);
		for (Obj *rest = CDDR(nums); rest != Nil; rest = CDR(rest)) {
		    Obj *tmp = cons(OP_ADD, Nil);
		    CDR(tail) = term_location(CAR(rest), env, tmp);
		    tail = tmp;
		}
		CDR(tail) = code;
		return ret;
	    }
	}
	if (PRIM_SUB == head) {
	    // (- x y z ...)
	    if (len == 1)
		error("Malformed -");
	    Obj *nums = CDR(expr);
	    if (len == 2) {
		Obj *term = CAR(nums);
		if (term->type == TINT)
		    return cons(OP_LDC, cons(make_int(-term->value), code));
		Obj *id = make_int(-1);
		return term_location(term, env, cons(OP_LDC, cons(id,
                    cons(OP_MUL, code))));
	    }
	    if (len >= 3) {
		Obj *x = CAR(nums);
		Obj *y = CADR(nums);
		Obj *tail = cons(OP_SUB, Nil);
		Obj *ret = term_location(y, env, tail);
		ret = term_location(x, env, ret);
		for (Obj *rest = CDDR(nums); rest != Nil; rest = CDR(rest)) {
		    Obj *tmp = cons(OP_SUB, Nil);
		    CDR(tail) = term_location(CAR(rest), env, tmp);
		    tail = tmp;
		}
		CDR(tail) = code;
		return ret;
	    }
	}
	if (PRIM_MUL == head) {
	    // (* x y z ...)
	    if (len == 1) {
		Obj *id = make_int(1);
		return cons(OP_LDC, cons(id, code));
	    }
	    Obj *nums = CDR(expr);
	    if (len == 2) {
		Obj *term = CAR(nums);
		if (term->type == TINT)
		    return cons(OP_LDC, cons(term, code));
		return term_location(term, env, code);
	    }
	    if (len >= 3) {
		Obj *x = CAR(nums);
		Obj *y = CADR(nums);
		Obj *tail = cons(OP_MUL, Nil);
		Obj *ret = term_location(y, env, tail);
		ret = term_location(x, env, ret);
		for (Obj *rest = CDDR(nums); rest != Nil; rest = CDR(rest)) {
		    Obj *tmp = cons(OP_MUL, Nil);
		    CDR(tail) = term_location(CAR(rest), env, tmp);
		    tail = tmp;
		}
		CDR(tail) = code;
		return ret;
	    }
	}
	if (PRIM_DIV == head) {
	    // (/ x y z ...)
	    if (len == 1)
		error("Malformed /");
	    Obj *nums = CDR(expr);
	    if (len == 2) {
		Obj *term = CAR(nums);
		Obj *ret = cons(OP_LDC, cons(make_int(1), Nil));
		if (term->type == TINT) {
		    CDDR(ret) = cons(OP_LDC, cons(term, cons(OP_DIV, code)));
		    return ret;
		}
		CDDR(ret) = term_location(term, env, cons(OP_DIV, code));
		return ret;
	    }
	    if (len >= 3) {
		Obj *x = CAR(nums);
		Obj *y = CADR(nums);
		Obj *tail = cons(OP_DIV, Nil);
		Obj *ret = term_location(y, env, tail);
		ret = term_location(x, env, ret);
		for (Obj *rest = CDDR(nums); rest != Nil; rest = CDR(rest)) {
		    Obj *tmp = cons(OP_DIV, Nil);
		    CDR(tail) = term_location(CAR(rest), env, tmp);
		    tail = tmp;
		}
		CDR(tail) = code;
		return ret;
	    }
	}
	if (PRIM_MOD == head) {
	    // (% x y)
	    if (len != 3)
		error("Malformed %");
	    Obj *x = CADR(expr);
	    Obj *y = CADDR(expr);
	    return comp(x, env, comp(y, env, cons(OP_MOD, code)));
	}
	if (PRIM_LT == head) {
	    // (< x y)
	    if (len != 3)
		error("Malformed <");
	    Obj *x = CADR(expr);
	    Obj *y = CADDR(expr);
	    return comp(x, env, comp(y, env, cons(OP_LT, code)));
	}
	if (PRIM_LE == head) {
	    // (<= x y)
	    if (len != 3)
		error("Malformed <=");
	    Obj *x = CADR(expr);
	    Obj *y = CADDR(expr);
	    return comp(x, env, comp(y, env, cons(OP_LE, code)));
	}
	if (PRIM_NEQ == head) {
	    // (= x y)
	    if (len != 3)
		error("Malformed =");
	    Obj *x = CADR(expr);
	    Obj *y = CADDR(expr);
	    return comp(x, env, comp(y, env, cons(OP_NEQ, code)));
	}
	if (PRIM_ATOM == head) {
	    // (atom x)
	    if (len != 2)
		error("Malformed atom");
	    Obj *x = CADR(expr);
	    return comp(x, env, cons(OP_ATOM, code));
	}
	if (PRIM_EQ == head) {
	    // (eq x y)
	    if (len != 3)
		error("Malformed eq");
	    Obj *x = CADR(expr);
	    Obj *y = CADDR(expr);
	    return comp(x, env, comp(y, env, cons(OP_EQ, code)));
	}
//	if (CDR(expr)->type != TCELL)
//	    error("argument must be a list");
	// calling function
	return complis(CDR(expr),
		       env,
		       cons(OP_ARGS,
			    cons(length(CDR(expr)),
				 comp(head,
				      env,
				      cons(OP_APP, code)))));
    default:
	error("comp: Unkown error\n");
        // NOTREACHED
        return Nil;
    }
}

static Obj *compile(Obj *expr) {
    Obj *code = cons(OP_STOP, Nil);
    return comp(expr, Nil, code);
}

// -----------------------------------------------------------------------------
// 7. Entry point
// -----------------------------------------------------------------------------

static void add_operation(Obj *opc, Op *fn) {
    Obj *op = make_operation(fn);
    Opcodes = acons(opc, op, Opcodes);
}

static void define_operations(void) {
    add_operation(OP_LD,   op_ld);
    add_operation(OP_LDC,  op_ldc);
    add_operation(OP_LDG,  op_ldg);
    add_operation(OP_LDF,  op_ldf);
    add_operation(OP_ARGS, op_args);
    add_operation(OP_APP,  op_app);
    add_operation(OP_RTN,  op_rtn);
    add_operation(OP_DUM,  op_dum);
    add_operation(OP_RAP,  op_rap);
    add_operation(OP_SEL,  op_sel);
    add_operation(OP_JOIN, op_join);
    add_operation(OP_POP,  op_pop);
    add_operation(OP_DEF,  op_def);
    add_operation(OP_ADD,  op_add);
    add_operation(OP_SUB,  op_sub);
    add_operation(OP_MUL,  op_mul);
    add_operation(OP_DIV,  op_div);
    add_operation(OP_MOD,  op_mod);
    add_operation(OP_CONS, op_cons);
    add_operation(OP_CAR,  op_car);
    add_operation(OP_CDR,  op_cdr);
    add_operation(OP_LT,   op_lt);
    add_operation(OP_LE,   op_le);
    add_operation(OP_NEQ,  op_neq);
    add_operation(OP_ATOM, op_atom);
    add_operation(OP_EQ,   op_eq);
    add_operation(OP_STOP, op_stop);
}

static void add_variable(Obj *sym, Obj *val) {
    Globals = acons(sym, val, Globals);
}

static void define_constants(void) {
    add_variable(ATOM_T,   True);
    add_variable(ATOM_NIL, Nil);
}

#define VERBOSE_MODE 1

int main(void) {
    Nil     = make_special(TNIL);
    Dot     = make_special(TDOT);
    Cparen  = make_special(TCPAREN);
    True    = make_special(TTRUE);
    Closure = make_special(TCLOSURE);

    Symbols = Nil;
    Globals = Nil;
    Opcodes = Nil;

    define_operations();
    define_constants();

    Obj *s, *e, *c, *d;
    for (;;) {
	s = e = d = Nil;
	printf("> ");
        Obj *expr = read();
        if (!expr)
            return 0;
        if (expr == Cparen)
            error("Stray close parenthesis");
        if (expr == Dot)
            error("Stray dot");
	c = compile(expr);
	if (VERBOSE_MODE) {
	    printf("  ;; compiled: ");
	    print(c);
	    putchar('\n');
	}
	vm(s, e, c, d);
    }
}
