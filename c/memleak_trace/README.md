# memleak_trace

A technique to trace memory allocation and find memory leak per thread.

nmparser.pl can generate a symbol table for all functions from an object file.

uClibc-0.9.31-z-memory-debug.patch is applied to uClibc to add hook for pthread_create(), malloc() and free().

The hook functions are implemented in memory.c to trace thread creation, memory allocation and deallocation.
