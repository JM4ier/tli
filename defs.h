#ifndef __LISP_DEFS_H__
#define __LISP_DEFS_H__

#include <stdint.h>

typedef int64_t ptr;
typedef int64_t i64;

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
        i64 value;
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
        ptr _data[2];
    };
    // number of references to this node, 0 == unused
    i64 refs;
    // kind of node
    i64 kind;
} node_t;

typedef struct
{
    ptr binding;
    ptr node;
    char name[16];
} sym_t;

#endif