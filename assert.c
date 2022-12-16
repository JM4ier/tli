#include "assert.h"

#include <stdio.h>
#include <stdlib.h>
#include <execinfo.h>

int *FOO = NULL;
void __print_backtrace__(void)
{
    void *funs[100] = {0};
    char **names = {0};

    int num_funs = backtrace(funs, 100);
    names = backtrace_symbols(funs, num_funs);

    printf("\n--- BACKTRACE(%d) ---\n", num_funs);

    for (int i = 0; i < num_funs; i++)
    {
        printf(" %s\n", names[i]);
    }

    ++*FOO;

    exit(-1);
}
