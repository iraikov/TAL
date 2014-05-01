
#ifndef CLIB_H
#define CLIB_H

#include <clib/support.h>

/* File descriptors */

#define STDIN  0
#define STDOUT 1
#define STDERR 2

extern void clib_close(int fd);

/* vector creation */
extern heap_address_t clib_iota (heap_address_t l, closure_t *f, double low, double high, double step);


#endif
