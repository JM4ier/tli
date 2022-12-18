#include <stdio.h>
#include <string.h>

#include "lisp.h"
#include "assert.h"

int is_string(ptr node);
int is_string(ptr node)
{
    if (kind(node) != T_CON)
        return 0;
    ptr head = get_head(node);
    ptr tail = get_tail(node);
    if (kind(head) != T_INT)
        return 0;
    int i = get_int(head);
    if (i >= 32 && i < 128)
    {
        if (kind(tail) == T_NIL)
            return 1;
        return is_string(tail);
    }
    return 0;
}

void print(ptr i)
{
    switch (kind(i))
    {
    case T_FUN:
        printf("<builtin fun>");
        return;
    case T_MAC:
        printf("<builtin macro>");
        return;
    case T_INT:
        printf("%ld", get_int(i));
        return;
    case T_NIL:
        printf("nil");
        return;
    case T_SYM:
        printf("%s", get_symbol_str(get_symbol(i)));
        return;
    case T_EMT:
        printf("<empty>");
        failwith("somehow managed to print non existent thing");
    case T_POO:
        printf("<?>");
        failwith("somehow managed to print garbage");
    case T_CON:
        break;
    default:
        printf("<unknown kind %ld>", kind(i));
        failwith("somehow managed to corrupt kind");
    }
    assert(kind(i) == T_CON);
    printf("(");
    ptr old_i = i;
    while (kind(i) == T_CON)
    {
        ptr head = get_head(i);
        assert(head != i);
        print(head);
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
    i = old_i;
    if (is_string(i))
    {
        printf("[\"");
        while (kind(i) == T_CON)
        {
            ptr node = get_head(i);
            char chr = (char)get_int(node);
            printf("%c", chr);
            i = get_tail(i);
        }
        printf("\"]");
    }
}

void println(ptr i)
{
    print(i);
    printf("\n");
}

void dump(void)
{
    printf("-===- DUMP BEGIN -===-\n");
    for (ptr i = 0; i < MEM_LEN; i++)
    {
        if (kind(i) == T_POO || kind(i) == T_EMT)
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
        if (strlen(get_symbol_str(s)))
        {
            printf(".%03ld: (%03ld) `%s`\n", s, get_symbol_binding(s), get_symbol_str(s));
        }
    }
    printf("-===- DUMP END -===-\n");
}
