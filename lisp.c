#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "lisp.h"
#include "assert.h"

#define LISP_LEN (1 << 20)

int main(void)
{
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
    dump();

    char *lisp_ptr = &lisp[0];
    char **cursor = &lisp_ptr;

    while (**cursor)
    {
        ptr parsed = parse(cursor);
        ptr evaled = eval(parsed);
        println(evaled);
    }

    return 0;
}
