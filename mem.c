#include <stdarg.h>
#include <string.h>
#include <stdlib.h>

#include "lisp.h"
#include "assert.h"

static node_t mem[MEM_LEN] = {0};
static ptr empty = 0;

static sym_t symbols[SYM_LEN] = {0};

static ptr sym_lambda = 0, sym_def = 0, sym_macro = 0, sym_unquote = 0;

int is_unquote(ptr i)
{
    return i == sym_unquote;
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

ptr elem(int idx, ptr node)
{
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
