#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define ptr int64_t
#define int int64_t

#define assert(x) do {\
    if (!(x)) {\
        printf("Assertion failed on line %d: `%s`\n", __LINE__, #x);\
        exit(-2);\
    }\
} while(0)

#define true 1
#define false 0

typedef union {
    int value;
    struct {
        ptr head, tail;
    };
    char *symbol;
    ptr next_free;
} node;

#define T_POO 0 // garbage value
#define T_NIL 1 // NIL
#define T_INT 2 // integer
#define T_CON 3 // cons, i.e. a pair
#define T_SYM 4 // symbol
#define T_EMT 5 // empty

typedef struct {
    // inlined node content
    node;
    // number of references to this node, 0 == unused
    int refs;
    // kind of node
    int kind;
} node_t;

static node_t mem[100000] = {0};
static ptr empty = 1;
static char symbols[10000] = {0};
static ptr symbols_end = 0;

char *kind_str(int kind) {
    char *p = "UNINIT\0NIL\0INT\0CONS\0SYM\0EMPTY";
    while (kind--) {
        while(*p++);
    }
    return p;
}

void init() {
    mem[0].kind = T_NIL;
    mem[0].refs = 999;

    int len = sizeof(mem) / sizeof(mem[0]);
    for (int i = 1; i < len; ++i) {
        mem[i].kind = T_EMT;
        if (i < len - 1) {
            mem[i].next_free = i+1;
        } else {
            mem[i].next_free = ~0;
        }
    }
}

ptr alloc() {
    if(mem[empty].kind != T_EMT) {
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

ptr new_int(int value) {
    ptr i = alloc();
    mem[i].kind = T_INT;
    mem[i].value = value;
    return i;
}

ptr new_cons(ptr head, ptr tail) {
    ptr i = alloc();
    mem[i].kind = T_CON;
    mem[i].head = head;
    mem[i].tail = tail;
    return i;
}

ptr new_nil() {
    return 0;
}

ptr new_symbol(char *symbol) {
    ptr i = alloc();
    mem[i].kind = T_SYM;
    mem[i].symbol = &symbols[symbols_end];

    // TODO: avoid duplicates
    while(symbols[symbols_end++] = *symbol++);
    return i;
}

int get_int(ptr i) {
    assert(mem[i].kind == T_INT);
    return mem[i].value;
}

ptr get_head(ptr i) {
    assert(mem[i].kind == T_CON);
    return mem[i].head;
}

ptr get_tail(ptr i) {
    assert(mem[i].kind == T_CON);
    return mem[i].tail;
}

char *get_symbol(ptr i) {
    assert(mem[i].kind == T_SYM);
    return mem[i].symbol;
}

ptr get_nil(ptr i) {
    assert(mem[i].kind == T_NIL);
    return 0;
}

void print(ptr i) {
    switch (mem[i].kind) {
        case T_INT:
            printf("%d", get_int(i));
            return;
        case T_NIL:
            printf("nil");
            return;
        case T_SYM:
            printf("%s", get_symbol(i));
            return;
    }
    assert(mem[i].kind == T_CON);
    printf("(");
    while(mem[i].kind == T_CON) {
        print(get_head(i));
        i = get_tail(i);
        if (mem[i].kind != T_NIL) {
            printf(" ");
        }
    }
    if (mem[i].kind != T_NIL) {
        printf(". ");
        print(i);
    }
    printf(")");
}

void println(ptr i) {
    print(i);
    printf("\n");
}

int main() {
    init();
    printf("%s %s\n", kind_str(T_NIL), kind_str(T_SYM));
    ptr sym = new_symbol("hello");
    println(
        new_cons(new_int(42),
        new_cons(new_int(43),
        new_cons(new_nil(),
        new_cons(sym,
        new_nil())))));
    return 0;
}