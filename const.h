#ifndef __CONST_H__
#define __CONST_H__

// number of "values" that can be stored in the interpreter
#define MEM_LEN 100000

// maximal usage in percent:
// this is to avoid repeatedly running the garbage collector to free 
// only a few values, and instead notice that we should increase the memory size.
#define MAX_MEMORY_USAGE 60

// number of symbols that can be defined
#define SYM_LEN 1024
#define SYM_SIZE 16

// number of builtin functions that can be defined
#define MAX_BUILTINS 100

#endif
