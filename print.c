#include <stdio.h>
#include "lisp.h"
#include "assert.h"

void print(ptr i)
{
    if (i < 0)
    {
        printf("<builtin>");
        return;
    }
    switch (kind(i))
    {
    case T_INT:
        printf("%ld", get_int(i));
        return;
    case T_NIL:
        printf("nil");
        return;
    case T_SYM:
        printf("%s", get_symbol_str(get_symbol(i)));
        return;
    default:
        break;
    }
    assert(kind(i) == T_CON);
    printf("(");
    while (kind(i) == T_CON)
    {
        print(get_head(i));
        i = get_tail(i);
        if (kind(i) != T_NIL)
        {
            printf(" ");
        }
    }
    if (kind(i) != T_NIL)
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
