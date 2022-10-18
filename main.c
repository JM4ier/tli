#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "defs.h"

static node_t mem[100000] = {0};
static ptr empty = 2;
#define SYM_LEN 1024
static sym_t symbols[SYM_LEN] = {0};

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
    mem[0].refs = 999;

    mem[1].kind = T_INT;
    mem[1].refs = 999;
    mem[1].value = 1;

    int len = sizeof(mem) / sizeof(mem[0]);
    for (int i = 2; i < len; ++i)
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

    ptr i = alloc();
    mem[i].kind = T_SYM;

    for (ptr k = 0; k < SYM_LEN; k++)
    {
        if (strlen(symbols[k].name) == 0)
        {
            int len = strlen(symbol);
            assert(len > 0 && len < 16);
            strcpy(symbols[k].name, symbol);
            mem[i].symbol = k;
            return i;
        }
        else if (!strcmp(symbols[k].name, symbol))
        {
            mem[i].symbol = k;
            return i;
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

void print(ptr i)
{
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

ptr eval(ptr i)
{
    switch (mem[i].kind)
    {
    case T_NIL:
    case T_INT:
        return i;
    case T_SYM:
        assert(false && "pls implement symbol lookup");
    case T_CON:
        assert(false && "pls implement function evaluation");
    default:
        assert(false && "unreachable");
    }
}

int main()
{
    init();
    printf("%s %s\n", kind_str(T_NIL), kind_str(T_SYM));
    ptr sym = new_symbol("hello");
    println(
        new_cons(new_int(42),
                 new_cons(new_int(43),
                          new_cons(new_nil(),
                                   new_cons(sym,
                                            new_nil())))));
    assert(mem[lt(
                   new_cons(new_int(42),
                            new_cons(new_int(43),
                                     new_nil())))]
               .kind != T_NIL);
    assert(eq(new_symbol("hello"), new_symbol("hello")));
    println(eval(new_int(42)));
    return 0;
}