#! /bin/sh

gcc -O3 *.c *.s \
    -o lisp.bin \
    -std=c17 -pedantic -Wall -Wshadow -Wpointer-arith -Wcast-qual \
        -Wstrict-prototypes -Wmissing-prototypes

./lisp.bin

