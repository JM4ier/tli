#ifndef __LISP_DEFS_H__
#define __LISP_DEFS_H__

#include <stdint.h>
#include "const.h"

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
#define T_FUN 6 // builtin function
#define T_MAC 7 // builtin macro

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

        // if builtin function or macro, function pointer
        ptr (*builtin)(ptr);

        // if not in use, point to next free node
        ptr next_free;
        ptr _data[2];
    };
    // GC data
    i64 gc;
    // kind of node
    i64 kind;
} node_t;

typedef struct
{
    ptr binding;
    ptr node;
    char name[SYM_SIZE];
} sym_t;

void init(void);

void new_binding(ptr symbol, ptr expression);
void register_builtins(void);

// construct new nodes
ptr new_int(i64 value);
ptr new_cons(ptr head, ptr tail);
ptr new_nil(void);
ptr new_list(int len, ...);
ptr new_true(void);
ptr new_symbol(char *symbol);
ptr new_builtin(ptr (*fun)(ptr), char *sym, int kind);
ptr quoted(ptr i);

// garbage collection
void gc(void);

// get kind of data
i64 kind(ptr i);

// get data out of nodes, needs to be correct kind
i64 get_int(ptr i);
ptr get_symbol(ptr i);
ptr get_nil(ptr i);
ptr get_head(ptr i);
ptr get_tail(ptr i);
ptr elem(int idx, ptr node);
char *get_symbol_str(ptr s);
ptr get_symbol_binding(ptr s);
ptr (*get_fn_ptr(ptr i))(ptr);

int mem_usage(void);

// eval an expression
// might have side effects
ptr eval(ptr i);

int is_quote(ptr i);
int is_quasiquote(ptr i);
int is_unquote(ptr i);
int is_lambda(ptr i);
int is_macro(ptr i);
int is_functionlike(ptr i);
int is_definition(ptr i);
int is_partial_app(ptr i);
int is_pragma(ptr i);

void print(ptr i);
void println(ptr i);
void dump(void);

// parsing
ptr parse(char **input);
void strip(char **input);

int get_iter(void);

extern i64 *stack_top;

extern void stack_search(void);
void stack_search_impl(void);

#endif
