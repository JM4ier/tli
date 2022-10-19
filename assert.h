#ifndef __ASSERT_H__
#define __ASSERT_H__

#include <stdio.h>

void __print_backtrace__(void);

#define assert(x)                                                                 \
    do                                                                            \
    {                                                                             \
        if (!(x))                                                                 \
        {                                                                         \
            printf("\nAssertion failed at %s:%d `%s`\n", __FILE__, __LINE__, #x); \
            __print_backtrace__();                                                \
        }                                                                         \
    } while (0)

#define failwith(x)                                                   \
    do                                                                \
    {                                                                 \
        printf("Code failed at %s:%d `%s`\n", __FILE__, __LINE__, x); \
        __print_backtrace__();                                        \
    } while (1)

#endif
