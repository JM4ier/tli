#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#include "lisp.h"
#include "assert.h"

static node_t mem[MEM_LEN] = {0};
static ptr empty = 0;

static sym_t symbols[SYM_LEN] = {0};

/*
nodes that are reserved for builtin use
should never be GC'ed
*/
static ptr builtin_use;

#define make(name)              \
    static ptr sym_##name = 0;  \
    int is_##name(ptr i)        \
    {                           \
        return i == sym_##name; \
    }
make(quote)
make(unquote)
make(quasiquote)
make(lambda)
make(macro)
make(definition)
make(partial_app)
make(pragma)
#undef make

#define UNBOUND 2

int is_functionlike(ptr i)
{
    return i == sym_lambda || i == sym_macro;
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
    make_sym(sym_definition, "def");
    make_sym(sym_macro, "m\\");
    make_sym(sym_quote, "quote");
    make_sym(sym_unquote, "unquote");
    make_sym(sym_quasiquote, "quasiquote");
    make_sym(sym_partial_app, "..");
    make_sym(sym_pragma, "pragma");
#undef make_sym
}

/*
reads the input file
*/
void read_input(void)
{
    FILE *f = fopen("input.txt", "rb");
    assert(f);

    fseek(f, 0, SEEK_END);
    size_t fsize = (size_t)ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buf = malloc(fsize + 1);
    assert(buf);

    fread(buf, fsize, 1, f);
    fclose(f);
    buf[fsize] = 0;

    ptr input = new_nil();
    for (char *cursor = buf + fsize - 1; cursor >= buf; cursor--)
    {
        input = new_cons(new_int(*cursor), input);
    }
    new_binding(new_symbol("input"), input);

    free(buf);
}

#define MEM_UNINIT 0
#define MEM_INITIALIZING 1
#define MEM_INITIALIZED 2
static int initialized = MEM_UNINIT;

/*
initializes the interpreter
do NOT call twice
*/
void init(void)
{
    if (initialized)
    {
        failwith("already initialized or in the process of doing so");
    }
    initialized = MEM_INITIALIZING;

    mem[0].kind = T_NIL;

    mem[1].kind = T_INT;
    mem[1].value = 1;

    assert(UNBOUND == 2);
    mem[UNBOUND].kind = T_POO;

    empty = 3;
    for (int i = 3; i < MEM_LEN; ++i)
    {
        mem[i].kind = T_EMT;
        if (i < MEM_LEN - 1)
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
    read_input();
    initialized = MEM_INITIALIZED;
}

/* GC run count */
static int gen = 1;

/* marks values with a global binding as 'in use' */
static void mark_globals(void)
{
    for (ptr s = 0; s < SYM_LEN; s++)
    {
        if (symbols[s].name[0] != 0)
        {
            int binding = symbols[s].binding;
            mem[binding].gc = gen;
        }
    }
}

ptr *walker;

/*
searches the entire C stack for references to LISP values
if it finds any, it marks those values as 'in use'
*/
void stack_search_impl(void)
{
    ptr stack_bottom = 0;
    walker = &stack_bottom;
    while (++walker != stack_top)
    {
        if (*walker<MEM_LEN && * walker> 0)
        {
            mem[*walker].gc = gen;
        }
    }
}

static void mark_reachable(ptr i);

/*
marks a value as 'in use' if it isn't already marked as such
if it is a freshly marked value, it will also mark the child values
*/
static void maybe_mark(ptr i)
{
    if (mem[i].gc != gen)
    {
        mem[i].gc = gen;
        mark_reachable(i);
    }
}

/* marks descendants of the node as reachable */
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

/* marks all indirectly reachable nodes as used */
static void mark_all_reachable(void)
{
    for (ptr i = 0; i < MEM_LEN; i++)
    {
        mark_reachable(i);
    }
}

/*
marks all unreachable nodes as 'empty' and make them available to be reused
*/
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
    // printf("GC go brrrr... (%d%%)\n", usage);
    if (usage > MAX_MEMORY_USAGE || usage > 99)
    {
        printf("Out of memory.\n");
        exit(-1);
    }
}

/*
garbage collector to free up nodes
does not return if out of memory, instead will stop program
*/
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
    }

    ptr next = mem[empty].next_free;
    ptr new = empty;
    empty = next;

    node_t zero = {0};
    mem[new] = zero;
    mem[new].gc = gen;

    return new;
}

static void check(ptr i)
{
    if (i >= 0 && i < MEM_LEN)
    {
        if (mem[i].gc == ~0 && mem[i].kind == T_EMT)
        {
            printf("%ld ", i);
            println(i);
            failwith("we freed a node that is still in use :(");
        }
    }
}

// -- constructors for new lisp values -- //

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

ptr new_builtin(ptr (*fun)(ptr), char *sym, int kind)
{
    ptr i = alloc();
    assert(kind == T_FUN || kind == T_MAC);
    mem[i].kind = kind;
    mem[i].builtin = fun;
    ptr s = new_symbol(sym);
    new_binding(s, i);
    return i;
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

            symbols[k].binding = UNBOUND; // point to garbage
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
    assert(i >= 0);
    assert(i < MEM_LEN);
    return mem[i].kind;
}

i64 get_int(ptr i)
{
    check(i);
    assert(mem[i].kind == T_INT);
    return mem[i].value;
}

ptr (*get_fn_ptr(ptr i))(ptr)
{
    assert(kind(i) == T_FUN || kind(i) == T_MAC);
    return mem[i].builtin;
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
    sym_t *sym = &symbols[get_symbol(symbol)];
    if (sym->binding != UNBOUND) {
        printf("Definitions cannot be shadowed.\nOffending symbol: %s.\n", &sym->name[0]);
        assert(initialized == MEM_INITIALIZING);
    }
    sym->binding = expression;
}
