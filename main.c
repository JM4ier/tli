#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "defs.h"

#define MEM_LEN 100000
static node_t mem[MEM_LEN] = {0};
static ptr empty = 0;
#define SYM_LEN 1024
static sym_t symbols[SYM_LEN] = {0};

static ptr sym_lambda = 0;
static ptr sym_def = 0;

#define MAX_BUILTINS 100
static ptr (*builtins[MAX_BUILTINS])(ptr) = {0};
static int builtins_len = 1;

ptr new_symbol(char *);
void new_binding(ptr symbol, ptr expression);

void register_builtin(ptr (*fun)(ptr), char *sym)
{
    assert(builtins_len < MAX_BUILTINS);
    int idx = -builtins_len;
    builtins[builtins_len++] = fun;
    ptr s = new_symbol(sym);
    new_binding(s, idx);
}

char *kind_str(int kind)
{
    char *p = "UNINIT\0NIL\0INT\0CONS\0SYM\0EMPTY";
    while (kind--)
    {
        while (*p++)
            ;
    }
    return p;
}

void init()
{
    mem[0].kind = T_NIL;
    mem[0].refs = 1;

    mem[1].kind = T_INT;
    mem[1].refs = 1;
    mem[1].value = 1;

    mem[2].kind = T_POO;
    mem[2].refs = 1;

    int len = sizeof(mem) / sizeof(mem[0]);

    empty = 3;
    for (int i = 3; i < len; ++i)
    {
        mem[i].kind = T_EMT;
        if (i < len - 1)
        {
            mem[i].next_free = i + 1;
        }
        else
        {
            mem[i].next_free = ~0;
        }
    }

    sym_lambda = new_symbol(".\\");
    sym_def = new_symbol("def");

    register_int_builtins();
}

ptr alloc()
{
    if (mem[empty].kind != T_EMT)
    {
        // TODO: collect garbage
        printf("Out of mem.\n");
        exit(-1);
    }

    ptr next = mem[empty].next_free;
    ptr new = empty;
    empty = next;

    node_t zero = {0};
    mem[new] = zero;

    return new;
}

ptr new_int(int value)
{
    ptr i = alloc();
    mem[i].kind = T_INT;
    mem[i].value = value;
    return i;
}

ptr new_cons(ptr head, ptr tail)
{
    ptr i = alloc();
    mem[i].kind = T_CON;
    mem[i].head = head;
    mem[i].tail = tail;
    return i;
}

ptr new_nil()
{
    return 0;
}

ptr new_true()
{
    return 1;
}

ptr new_symbol(char *symbol)
{
    if (!strcmp(symbol, "nil") || !strcmp(symbol, "NIL"))
    {
        return 0;
    }

    for (ptr k = 0; k < SYM_LEN; k++)
    {
        if (strlen(symbols[k].name) == 0)
        {
            // we arrived at the section of unused symbols
            // we have not found the symbol in the existing table
            // we add a new symbol

            ptr i = alloc();
            mem[i].kind = T_SYM;
            mem[i].symbol = k;

            int len = strlen(symbol);
            assert(len > 0 && len < 16);

            strcpy(symbols[k].name, symbol);

            symbols[k].binding = 2; // point to garbage
            symbols[k].node = i;
            return i;
        }
        else if (!strcmp(symbols[k].name, symbol))
        {
            return symbols[k].node;
        }
    }

    assert(false && "Out of Symbols.");
}

int get_int(ptr i)
{
    assert(mem[i].kind == T_INT);
    return mem[i].value;
}

ptr get_head(ptr i)
{
    assert(mem[i].kind == T_CON);
    return mem[i].head;
}

ptr get_tail(ptr i)
{
    assert(mem[i].kind == T_CON);
    return mem[i].tail;
}

ptr get_symbol(ptr i)
{
    assert(mem[i].kind == T_SYM);
    return mem[i].symbol;
}

ptr get_nil(ptr i)
{
    assert(mem[i].kind == T_NIL);
    return 0;
}

void new_binding(ptr symbol, ptr expression)
{
    symbols[get_symbol(symbol)].binding = expression;
}

ptr eq(ptr a, ptr b)
{
    if (mem[a].kind != mem[b].kind)
        return new_nil();
    switch (mem[a].kind)
    {
    case T_NIL:
        return new_true();
    case T_SYM:
    case T_INT:
        return mem[a].value == mem[b].value ? new_true() : new_nil();
    case T_CON:
        return eq(get_head(a), get_head(b)) &&
               eq(get_tail(a), get_tail(b));
    default:
        assert(false && "unreachable");
    }
}

#define _ORD_(name, cmp)                                       \
    ptr name(ptr a)                                            \
    {                                                          \
        if (mem[a].kind == T_NIL)                              \
        {                                                      \
            return new_true();                                 \
        }                                                      \
        assert(mem[a].kind == T_CON);                          \
        ptr b = get_tail(a);                                   \
        if (mem[b].kind != T_NIL)                              \
        {                                                      \
            if (get_int(get_head(a)) cmp get_int(get_head(b))) \
            {                                                  \
                return name(b);                                \
            }                                                  \
            else                                               \
            {                                                  \
                return new_nil();                              \
            }                                                  \
        }                                                      \
        else                                                   \
        {                                                      \
            return new_true();                                 \
        }                                                      \
    }
_ORD_(lt, <)
_ORD_(gt, >)
_ORD_(lte, <=)
_ORD_(gte, >=)

#define _ARITH_(name, op, init)                \
    ptr name(ptr a)                            \
    {                                          \
        int val = init;                        \
        while (mem[a].kind == T_CON)           \
        {                                      \
            val = val op get_int(get_head(a)); \
            a = get_tail(a);                   \
        }                                      \
        return new_int(val);                   \
    }
_ARITH_(sum, +, 0)
_ARITH_(prod, *, 1)

#define _CMP_(name, _kind)\
    ptr name(ptr i)\
    {\
        i = get_head(i);\
        if (i < 0 || mem[i].kind != _kind) {\
            return new_nil();\
        } else {\
            return new_true();\
        }\
    }
_CMP_(is_nil, T_NIL)
_CMP_(is_int, T_INT)
_CMP_(is_sym, T_SYM)
_CMP_(is_pair, T_CON)

void register_int_builtins() {
    register_builtin(&lt, "<");
    register_builtin(&gt, ">");
    register_builtin(&lte, "<=");
    register_builtin(&gte, ">=");
    register_builtin(&sum, "+");
    register_builtin(&prod, "*");

    register_builtin(&is_nil, "nil?");
    register_builtin(&is_int, "int?");
    register_builtin(&is_sym, "sym?");
    register_builtin(&is_pair, "pair?");
}

void print(ptr i)
{
    if (i < 0) {
        printf("<builtin>");
        return;
    }
    switch (mem[i].kind)
    {
    case T_INT:
        printf("%d", get_int(i));
        return;
    case T_NIL:
        printf("nil");
        return;
    case T_SYM:
        printf("%s", symbols[get_symbol(i)].name);
        return;
    }
    assert(mem[i].kind == T_CON);
    printf("(");
    while (mem[i].kind == T_CON)
    {
        print(get_head(i));
        i = get_tail(i);
        if (mem[i].kind != T_NIL)
        {
            printf(" ");
        }
    }
    if (mem[i].kind != T_NIL)
    {
        printf(". ");
        print(i);
    }
    printf(")");
}

void println(ptr i)
{
    print(i);
    printf("\n");
}

ptr eval_elems(ptr is);

ptr beta_reduce(ptr code, ptr formal_args, ptr args)
{
    // TODO handle macro
    switch (mem[code].kind)
    {
    case T_NIL:
    case T_INT:
        return code;
    case T_SYM:
        for (ptr fa = formal_args, a = args; mem[fa].kind != T_NIL; fa = get_tail(fa), a = get_tail(a))
        {
            if (get_symbol(code) == get_symbol(get_head(fa)))
            {
                return get_head(a);
            }
        }
        return code;
    case T_CON:
        ptr head = get_head(code);
        ptr tail = get_tail(code);
        if (head == sym_lambda)
        {
            return code;
        }
        ptr new_head = beta_reduce(head, formal_args, args);
        ptr new_tail = beta_reduce(tail, formal_args, args);
        if (new_head == head && new_tail == tail)
        {
            return code;
        }
        else
        {
            return new_cons(new_head, new_tail);
        }
    default:
        assert(false && "unreachable");
    }
}

ptr eval(ptr i)
{
    if (i < 0) {
        return i;
    }
    switch (mem[i].kind)
    {
    case T_NIL:
    case T_INT:
        return i;
    case T_SYM:
        ptr sym = get_symbol(i);
        ptr bind = symbols[sym].binding;
        if (bind >= 0 && mem[bind].kind == T_POO)
        {
            printf("`%s` is unbound.\n", symbols[sym].name);
            assert(false);
        }
        return bind;
    case T_CON:
        if (get_head(i) == sym_lambda)
        {
            return i;
        }
        if (get_head(i) == sym_def) {
            ptr name = get_head(get_tail(i));
            ptr def = get_head(get_tail(get_tail(i)));
            new_binding(name, def);
            return 0;
        }
        ptr fun = eval(get_head(i));
        ptr args = eval_elems(get_tail(i));

        if (fun < 0) {
            return builtins[-fun](args);
        }

        assert(get_head(fun) == sym_lambda);

        ptr fun1 = get_tail(fun);
        assert(mem[fun1].kind == T_CON);
        ptr formal_args = get_head(fun1);

        ptr fun2 = get_tail(fun1);
        assert(mem[fun2].kind == T_CON);
        ptr fun_body = get_head(fun2);

        ptr reduced = beta_reduce(fun_body, formal_args, args);
        return eval(reduced);
    default:
        assert(false && "unreachable");
    }
}

ptr eval_elems(ptr is)
{
    if (mem[is].kind == T_CON)
    {
        ptr head = get_head(is);
        ptr tail = get_tail(is);

        ptr eval_head = eval(head);
        ptr eval_tail = eval_elems(tail);

        return new_cons(eval_head, eval_tail);
    }
    else
    {
        return eval(is);
    }
}

int is_numeric(char c)
{
    return c >= '0' && c <= '9';
}

int is_whitespace(char c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

void strip(char **i) {
    while (is_whitespace(**i)) {
        ++*i;
    }
}

int is_paren(char c)
{
    return c == '(' || c == ')';
}

ptr parse_list(char **input);

ptr parse(char **input)
{
    assert(**input && "unexpected EOF");
    strip(input);
    if (is_numeric(**input))
    {
        int num = 0;
        while (is_numeric(**input))
        {
            num *= 10;
            num += (**input) - '0';
            ++*input;
        }
        return new_int(num);
    }
    else if (**input == ')')
    {
        ++*input;
        return new_nil();
    }
    else if (**input == '(')
    {
        ++*input;
        return parse_list(input);
    }
    else
    {
        // symbol
        char *begin = *input;
        while (!is_whitespace(**input) && !is_paren(**input) && **input)
        {
            ++*input;
        }
        char buf[16] = {0};
        assert(*input - begin < 16);
        memcpy(buf, begin, *input - begin);
        ptr sym = new_symbol(buf);
        return sym;
    }
}

ptr parse_list(char **input)
{
    assert(**input && "unexpected EOF");
    strip(input);
    if (**input == ')')
    {
        ++*input;
        return new_nil();
    }
    else
    {
        ptr head = parse(input);
        ptr tail = parse_list(input);
        return new_cons(head, tail);
    }
}

ptr pars(char *input)
{
    char **cursor = &input;
    return parse(cursor);
}

void dump() {
    printf("-===- DUMP BEGIN -===-\n");
    for (int i = 0; i < MEM_LEN; i++) {
        if (mem[i].kind == T_POO || mem[i].kind == T_EMT) {
            continue;
        }
        printf("%04d: `", i);
        print(i);
        printf("`\n");
    }
    printf("\n");
    for (int s = 0; s < SYM_LEN; s++) {
        if (strlen(symbols[s].name)) {
            printf(".%03d: (%03d) `%s`\n", s, symbols[s].binding, symbols[s].name);
        }
    }
    printf("-===- DUMP END -===-\n");
}

signed main()
{

    #define LISP_LEN (1 << 20)

    // lisp source to be interpreted
    static char lisp[LISP_LEN] = {0};

    FILE *f = fopen("lisp", "rb");
    assert(f);

    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    assert(fsize < LISP_LEN);

    char *string = malloc(fsize + 1);
    fread(lisp, fsize, 1, f);
    fclose(f);
    lisp[fsize] = 0;

    init();
    dump();

    char *lisp_ptr = &lisp[0];
    char **cursor = &lisp_ptr;

    while (**cursor) {
        ptr parsed = parse(cursor);
        ptr evaled = eval(parsed);
        println(evaled);
    }

    return 0;
}

void test() {
    println(pars("(42 43 nil hello)"));
    assert(mem[lt(pars("(42 43 44)"))].kind != T_NIL);
    assert(eq(new_symbol("hello"), new_symbol("hello")));
    println(eval(new_int(42)));

    ptr sym = new_symbol("var");
    new_binding(sym, new_int(100));
    println(eval(sym));

    ptr sqr = pars("sqr");
    ptr fun = pars("(.\\ (x) x)");
    new_binding(sqr, fun);

    println(eval(fun));
    println(eval(new_cons(sqr, new_cons(new_int(5), new_nil()))));
}