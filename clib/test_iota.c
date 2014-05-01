/* -*-c-file-style: "k&r" -*- */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>


#include <clib/support.h>


extern heap_address_t clib_iota (heap_address_t l, closure_t *f, double low, double high, double step);

/* register block used for passing arguments between functions  */
value_t argblock_internal[32];
value_t *argblock;

/* special return closure  */
closure_t ret;

/* result register  */
value_t result;

/* fake state machine function */
void sm (code_address_t x)
{
sm_start:

     switch (x)
     {
     case 0: return;
     case 1: 
     {
	  result.RealNum = argblock[1].RealNum + 1.0;
	  x = ((closure_t *)(argblock[2].Heapval))->code;
	  goto sm_start;
     }; break;
     default: return;
     }
}

/* vector to be initialized by clib_iota */
struct
{
     size_t size;
     double *data;
} test_vector;

closure_t test_function;

int main (int argc, char **argv)
{
     int i; double low, high;
     argblock = argblock_internal;

     ret.code = 0;
     ret.env = NULL;

     test_function.code = 1;
     test_function.env = NULL;

     low = 0.1; high = 0.9;

     clib_iota ((heap_address_t)&test_vector, 
		&test_function,
		low, high, (high-low)/128);

     printf ("low = %g\n", low);
     printf ("high = %g\n", high);
     printf ("test_vector->size = %ld\n", test_vector.size);
     for (i = 0; i < test_vector.size; i++)
     {
	  printf ("test_vector->data[%d] = %g\n", i, test_vector.data[i]);
     }


     return 0;
}


