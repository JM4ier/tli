#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#include "lisp.h"
#include "assert.h"

static node_t mem[MEM_LEN] = {0};
static ptr empty = 0;

static sym_t symbols[SYM_LEN] = {0};

static ptr sym_lambda = 0, sym_def = 0, sym_macro = 0, sym_unquote = 0, sym_quote = 0, sym_quasiquote = 0;

// nodes that are reserved for builtin use
// should never be GC'ed
static ptr builtin_use;

int is_unquote(ptr i)
{
    return i == sym_unquote;
}

int is_quote(ptr i)
{
    return i == sym_quote;
}

int is_quasiquote(ptr i)
{
    return i == sym_quasiquote;
}

int is_lambda(ptr i)
{
    return i == sym_lambda;
}

int is_macro(ptr i)
{
    return i == sym_macro;
}

int is_functionlike(ptr i)
{
    return i == sym_lambda || i == sym_macro;
}

int is_definition(ptr i)
{
    return i == sym_def;
}

char *get_symbol_str(ptr s)
{
    return symbols[s].name;
}

ptr get_symbol_binding(ptr s)
{
    return symbols[s].binding;
}

static void init_builtin_symbols(void)
{
#define make_sym(var_name, sym) \
    var_name = new_symbol(sym); \
    new_binding(var_name, var_name)

    make_sym(sym_lambda, ".\\");
    make_sym(sym_def, "def");
    make_sym(sym_macro, "m\\");
    make_sym(sym_quote, "quote");
    make_sym(sym_unquote, "unquote");
    make_sym(sym_quasiquote, "quasiquote");
#undef make_sym
}

void init(void)
{
    mem[0].kind = T_NIL;

    mem[1].kind = T_INT;
    mem[1].value = 1;

    mem[2].kind = T_POO;

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

    init_builtin_symbols();
    register_builtins();
    builtin_use = empty;
}

static int gen = 1;
static void mark_globals(void)
{
    for (ptr s = 0; s < SYM_LEN; s++)
    {
        if (symbols[s].name[0] != 0)
        {
            int binding = symbols[s].binding;
            if (kind(binding) != T_NAT)
            {
                mem[binding].gc = gen;
            }
        }
    }
}

ptr *walker;
void stack_search_impl(void)
{
    ptr dummy = 0;
    walker = &dummy;
    while (++walker != stack_top) {
        if (*walker < MEM_LEN && *walker > 0)
        {
            mem[*walker].gc = gen;
        }
    }
}

static void mark_reachable(ptr i);
static void maybe_mark(ptr i)
{
    if (i < 0) return;
    if (mem[i].gc != gen)
    {
        mem[i].gc = gen;
        mark_reachable(i);
    }
}

static void mark_reachable(ptr i)
{
    if (mem[i].gc != gen)
    {
        return;
    }
    if (kind(i) == T_CON)
    {
        maybe_mark(get_head(i));
        maybe_mark(get_tail(i));
    }
}

static void mark_all_reachable(void)
{
    for (ptr i = 0; i < MEM_LEN; i++)
    {
        mark_reachable(i);
    }
}

static void reconstruct_empty_list(void)
{
    ptr prev_empty = 0;
    int free_memory = 0;
    for (ptr i = builtin_use; i < MEM_LEN; i++)
    {
        if (mem[i].gc == gen || kind(i) == T_SYM)
        {
            continue;
        }
        mem[i].kind = T_EMT;
        mem[i].gc = ~0;
        free_memory++;
        if (prev_empty)
        {
            mem[i].next_free = prev_empty;
        }
        else
        {
            mem[i].next_free = i;
        }
        prev_empty = i;
    }
    empty = prev_empty;

    int usage = 100 - 100 * free_memory / MEM_LEN;
    printf("GC go brrrr... (%d)%%\n", usage);
    if (usage > MAX_MEMORY_USAGE) {
        printf("Out of memory.\n");
        exit(-1);
    }
}

void gc(void)
{
    gen = (gen + 1) & ((~0) >> 1);
    mark_globals();
    stack_search();
    mark_all_reachable();
    reconstruct_empty_list();
}

static ptr alloc(void)
{
    if (kind(empty) != T_EMT)
    {
        gc();
        if (kind(empty) != T_EMT)
        {
            failwith("Out of Memory.");
        }
    }

    ptr next = mem[empty].next_free;
    ptr new = empty;
    empty = next;

    node_t zero = {0};
    mem[new] = zero;
    mem[new].gc = gen;

    return new;
}

static void check(ptr i) {
    if (mem[i].gc == ~0 && mem[i].kind == T_EMT)
    {
        printf("%ld ", i);
        println(i);
        failwith("we freed a node that is still in use :(");
    }
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
    check(head);
    check(tail);
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

    ptr args[len];

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
    check(i);
    switch (kind(i))
    {
    case T_CON:
    case T_SYM:
        return new_list(2, new_symbol("quote"), i);
    default:
        return i;
    }
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

i64 kind(ptr i)
{
    if (i < 0)
    {
        return T_NAT;
    }
    else
    {
        return mem[i].kind;
    }
}

i64 get_int(ptr i)
{
    check(i);
    assert(mem[i].kind == T_INT);
    return mem[i].value;
}

ptr get_head(ptr i)
{
    check(i);
    assert(mem[i].kind == T_CON);
    return mem[i].head;
}

ptr get_tail(ptr i)
{
    check(i);
    assert(mem[i].kind == T_CON);
    return mem[i].tail;
}

ptr elem(int idx, ptr node)
{
    check(node);
    if (idx)
    {
        return elem(idx - 1, get_tail(node));
    }
    else
    {
        return get_head(node);
    }
}

ptr get_symbol(ptr i)
{
    check(i);
    assert(mem[i].kind == T_SYM);
    return mem[i].symbol;
}

ptr get_nil(ptr i)
{
    check(i);
    assert(mem[i].kind == T_NIL);
    return 0;
}

void new_binding(ptr symbol, ptr expression)
{
    symbols[get_symbol(symbol)].binding = expression;
}
