#ifndef __LISP_DEFS_H__
#define __LISP_DEFS_H__

#include <stdint.h>

#define ptr int64_t
#define int int64_t

#define assert(x)                                                               \
    do                                                                          \
    {                                                                           \
        if (!(x))                                                               \
        {                                                                       \
            printf("Assertion failed at %s:%d `%s`\n", __FILE__, __LINE__, #x); \
            exit(-2);                                                           \
        }                                                                       \
    } while (0)

#define true 1
#define false 0

#define T_POO 0 // garbage value
#define T_NIL 1 // NIL
#define T_INT 2 // integer
#define T_CON 3 // cons, i.e. a pair
#define T_SYM 4 // symbol
#define T_EMT 5 // empty

typedef struct
{
    // Contents of a node
    union
    {
        // integer value
        int value;
        struct
        {
            // head of cons
            ptr head;
            // tail of cons
            ptr tail;
        };
        // pointer to the symbol
        ptr symbol;

        // if not in use, point to next free node
        ptr next_free;
    };
    // number of references to this node, 0 == unused
    int refs;
    // kind of node
    int kind;
} node_t;

typedef struct
{
    ptr binding;
    char name[16];
} sym_t;

#endif