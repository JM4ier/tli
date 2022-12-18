#! /bin/sh

rm lisp.bin

gcc -g -Oz *.c *.s \
    -o lisp.bin \
    -std=c17 -pedantic -Wall -Wshadow -Wpointer-arith -Wcast-qual \
        -Wstrict-prototypes

./lisp.bin

