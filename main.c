#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "defs.h"
#include "assert.h"

#define MEM_LEN 100000
static node_t mem[MEM_LEN] = {0};
static ptr empty = 0;
#define SYM_LEN 1024
static sym_t symbols[SYM_LEN] = {0};

static ptr sym_lambda = 0;
static ptr sym_def = 0;
static ptr sym_macro = 0;
static ptr sym_unquote = 0;

#define MAX_BUILTINS 100
static ptr (*builtins[MAX_BUILTINS])(ptr) = {0};
static int builtins_len = 1;

ptr new_symbol(char *);
void new_binding(ptr symbol, ptr expression);
ptr eval_elems(ptr is);
ptr eval(ptr i);
void register_builtins(void);

void new_builtin(ptr (*fun)(ptr), char *sym)
{
    assert(builtins_len < MAX_BUILTINS);
    int idx = -builtins_len;
    builtins[builtins_len++] = fun;
    ptr s = new_symbol(sym);
    new_binding(s, idx);
}

void init(void)
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
    sym_macro = new_symbol("m\\");
    sym_unquote = new_symbol("unquote");

    register_builtins();
}

ptr alloc(void)
{
    if (mem[empty].kind != T_EMT)
    {
        // TODO: collect garbage
        failwith("Out of memory.");
    }

    ptr next = mem[empty].next_free;
    ptr new = empty;
    empty = next;

    node_t zero = {0};
    mem[new] = zero;

    return new;
}

ptr new_int(i64 value)
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

ptr new_nil(void)
{
    return 0;
}

ptr new_list(int len, ...)
{
    va_list vargs;
    va_start(vargs, len);

    ptr *args = malloc(len * sizeof(ptr));

    for (int i = 0; i < len; i++)
    {
        args[i] = va_arg(vargs, ptr);
    }

    ptr list = new_nil();

    for (int i = len - 1; i >= 0; i--)
    {
        list = new_cons(args[i], list);
    }

    return list;
}


ptr new_true(void)
{
    return 1;
}

ptr quoted(ptr i)
{
    return new_list(2, new_symbol("quote"), i);
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

            int len = (int)strlen(symbol);
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

    failwith("Out of Symbols.");
}

i64 get_int(ptr i)
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
    if (a == b)
    {
        return new_true();
    }
    if (a < 0 || b < 0 || mem[a].kind != mem[b].kind)
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
        failwith("unreachable");
    }
}

#define _ORD_(name, cmp)                                       \
    ptr name(ptr a)                                            \
    {                                                          \
        a = eval_elems(a);                                     \
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
        a = eval_elems(a);                     \
        i64 val = init;                        \
        while (mem[a].kind == T_CON)           \
        {                                      \
            val = val op get_int(get_head(a)); \
            a = get_tail(a);                   \
        }                                      \
        return new_int(val);                   \
    }
_ARITH_(sum, +, 0)
_ARITH_(prod, *, 1)

#define _CMP_(name, _kind)                 \
    ptr name(ptr i)                        \
    {                                      \
        i = eval_elems(i);                 \
        i = get_head(i);                   \
        if (i < 0 || mem[i].kind != _kind) \
        {                                  \
            return new_nil();              \
        }                                  \
        else                               \
        {                                  \
            return new_true();             \
        }                                  \
    }
_CMP_(is_nil, T_NIL)
_CMP_(is_int, T_INT)
_CMP_(is_sym, T_SYM)
_CMP_(is_pair, T_CON)

ptr is_list(ptr i)
{
    i = eval_elems(i);
    i = get_head(i);
    while (i >= 0 && mem[i].kind == T_CON)
    {
        i = get_tail(i);
    }
    if (i < 0 || mem[i].kind != T_NIL)
    {
        return new_nil();
    }
    else
    {
        return new_true();
    }
}

ptr eval_quote(ptr i)
{
    return get_head(i);
}

ptr apply_quasiquote(ptr i)
{
    if (i < 0)
    {
        return i;
    }
    switch (mem[i].kind)
    {
    case T_SYM:
    case T_NIL:
    case T_INT:
        return i;
    case T_CON:
    {
        ptr head = get_head(i);
        ptr tail = get_tail(i);
        if (head == sym_unquote)
        {
            return eval(get_head(tail));
        }
        else
        {
            ptr new_head = apply_quasiquote(head);
            ptr new_tail = apply_quasiquote(tail);
            if (new_head == head && new_tail == tail)
            {
                return i;
            }
            else
            {
                return new_cons(new_head, new_tail);
            }
        }
    }
    default:
        failwith("unreachable");
    }
}
ptr eval_quasiquote(ptr i)
{
    if (i < 0)
    {
        return i;
    }
    else
    {
        i = get_head(i);
        return apply_quasiquote(i);
    }
}

ptr eval_cond(ptr i)
{
    if (i < 0 || mem[i].kind == T_NIL)
    {
        return i;
    }
    else
    {
        assert(mem[i].kind == T_CON);

        ptr branch = get_head(i);
        ptr rest = get_tail(i);

        ptr cond = get_head(branch);
        ptr code = get_head(get_tail(branch));

        cond = eval(cond);
        if (mem[cond].kind == T_NIL)
        {
            return eval_cond(rest);
        }
        else
        {
            return eval(code);
        }
    }
}

void register_builtins()
{
    new_builtin(&lt, "<");
    new_builtin(&gt, ">");
    new_builtin(&lte, "<=");
    new_builtin(&gte, ">=");
    new_builtin(&sum, "+");
    new_builtin(&prod, "*");

    new_builtin(&is_nil, "nil?");
    new_builtin(&is_int, "int?");
    new_builtin(&is_sym, "sym?");
    new_builtin(&is_pair, "pair?");
    new_builtin(&is_list, "list?");
    new_builtin(&eval_quote, "quote");
    new_builtin(&eval_quasiquote, "quasiquote");
    new_builtin(&eval_cond, "cond");
}

void print(ptr i)
{
    if (i < 0)
    {
        printf("<builtin>");
        return;
    }
    switch (mem[i].kind)
    {
    case T_INT:
        printf("%ld", get_int(i));
        return;
    case T_NIL:
        printf("nil");
        return;
    case T_SYM:
        printf("%s", symbols[get_symbol(i)].name);
        return;
    default:
        break;
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

ptr beta_reduce(ptr code, ptr formal_args, ptr args)
{
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
                return quoted(get_head(a));
            }
        }
        return code;
    case T_CON:
    {
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
    }
    default:
        failwith("unreachable");
    }
}

ptr eval(ptr i)
{
    if (i < 0)
    {
        return i;
    }
    switch (mem[i].kind)
    {
    case T_NIL:
    case T_INT:
        return i;
    case T_SYM:
    {
        ptr sym = get_symbol(i);
        ptr bind = symbols[sym].binding;
        if (bind >= 0 && mem[bind].kind == T_POO)
        {
            printf("`%s` is unbound.\n", symbols[sym].name);
            failwith("");
        }
        return bind;
    }
    case T_CON:
    {
        ptr head = get_head(i);
        if (head == sym_lambda || head == sym_macro)
        {
            return i;
        }
        if (head == sym_def)
        {
            ptr name = get_head(get_tail(i));
            ptr def = get_head(get_tail(get_tail(i)));
            new_binding(name, def);
            return 0;
        }
        ptr fun = eval(head);
        ptr args = get_tail(i);

        if (fun < 0)
        {
            // builtins behave like macros by default, evaluation must be done by the function itself
            return builtins[-fun](args);
        }

        ptr fun_head = get_head(fun);

        assert(fun_head == sym_lambda || fun_head == sym_macro);

        if (fun_head == sym_lambda)
        {
            // argument evaluation
            args = eval_elems(args);
        }

        ptr fun1 = get_tail(fun);
        assert(mem[fun1].kind == T_CON);
        ptr formal_args = get_head(fun1);

        ptr fun2 = get_tail(fun1);
        assert(mem[fun2].kind == T_CON);
        ptr fun_body = get_head(fun2);

        ptr reduced = beta_reduce(fun_body, formal_args, args);

        if (fun_head == sym_macro)
        {
            // macro expansion
            reduced = eval(reduced);
        }

        return eval(reduced);
    }
    default:
        failwith("unreachable");
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

void strip(char **i)
{
    while (is_whitespace(**i))
    {
        ++*i;
    }
}

int is_paren(char c)
{
    return c == '(' || c == ')';
}

int is_quoting(char c)
{
    return c == '#' || c == '\'' || c == '`';
}

ptr parse_list(char **input);

ptr parse(char **input)
{
    assert(**input && "unexpected EOF");
    strip(input);
    if (is_numeric(**input))
    {
        i64 num = 0;
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
    else if (is_quoting(**input))
    {
        char *sym = 0;
        switch (**input)
        {
        case '\'':
            sym = "quote";
            break;
        case '`':
            sym = "quasiquote";
            break;
        case '#':
            sym = "unquote";
            break;
        default:
            failwith("unknown quote");
        }
        ++*input;
        ptr symbol = new_symbol(sym);
        return new_list(2, symbol, parse(input));
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
        memcpy(buf, begin, (size_t)(*input - begin));
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

void dump(void)
{
    printf("-===- DUMP BEGIN -===-\n");
    for (ptr i = 0; i < MEM_LEN; i++)
    {
        if (mem[i].kind == T_POO || mem[i].kind == T_EMT)
        {
            continue;
        }
        printf("%04ld: `", i);
        print(i);
        printf("`\n");
    }
    printf("\n");
    for (ptr s = 0; s < SYM_LEN; s++)
    {
        if (strlen(symbols[s].name))
        {
            printf(".%03ld: (%03ld) `%s`\n", s, symbols[s].binding, symbols[s].name);
        }
    }
    printf("-===- DUMP END -===-\n");
}

signed main(void)
{
#define LISP_LEN (1 << 20)

    // lisp source to be interpreted
    static char lisp[LISP_LEN] = {0};

    FILE *f = fopen("lisp", "rb");
    assert(f);

    fseek(f, 0, SEEK_END);
    size_t fsize = (size_t)ftell(f);
    fseek(f, 0, SEEK_SET);

    assert(fsize < LISP_LEN);

    fread(lisp, fsize, 1, f);
    fclose(f);
    lisp[fsize] = 0;

    init();
    dump();

    char *lisp_ptr = &lisp[0];
    char **cursor = &lisp_ptr;

    while (**cursor)
    {
        ptr parsed = parse(cursor);
        ptr evaled = eval(parsed);
        println(evaled);
    }

    return 0;
}

void test(void)
{
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