#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "lisp.h"
#include "assert.h"

#define LISP_LEN (1 << 20)

static int iter = 1;

int get_iter(void)
{
    return iter;
}

i64 *stack_top;

int main(void)
{
    i64 dummy = 0xC0FFEE;
    stack_top = &dummy;

    // lisp source to be interpreted
    static char lisp[LISP_LEN] = {0};

    FILE *f = fopen("lisp", "rb");
    assert(f);

    fseek(f, 0, SEEK_END);
    size_t fsize = (size_t)ftell(f);
    fseek(f, 0, SEEK_SET);

    assert(fsize < LISP_LEN);

    fread(lisp, fsize, 1, f);
    fclose(f);
    lisp[fsize] = 0;

    init();
    // dump();

    char *lisp_ptr = &lisp[0];
    char **cursor = &lisp_ptr;

    while (**cursor)
    {
        ptr parsed = parse(cursor);
        ptr evaled = eval(parsed);
        println(evaled);
        strip(cursor);
        iter++;
    }

    gc();

    int memory = mem_usage();
    char *unit[] = {"", "K", "M", "G", "T"};
    char **mem_unit = &unit[0];
    while (memory >= (1 << 13))
    {
        mem_unit++;
        memory /= 1024;
    }

    printf("We used %d%sB of memory for the lisp values\n", memory, *mem_unit);

    return 0;
}
