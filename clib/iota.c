/* -*-c-file-style: "k&r" -*- */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#include <clib/support.h>
#include <clib/interface.h>


/* iota function. Creates a vector of real numbers */
heap_address_t clib_iota (heap_address_t l, closure_t *f, double low, double high, double step)
{

     double x;
     
     long int dimension, i;
     
     struct
     {
	  size_t  size;
	  double *data;
     } *k;
     
     k = l;
     
     assert (k->size == 0);
     assert (k->data == NULL);
     
     if (!((high - low) > 0.0))
     {
	  clib_raise_exn (clib_make_exn(__FILE__,__LINE__,"iota: high  must be > low"));
     }
     
     if ((high - low) < step)
     {
	  clib_raise_exn (clib_make_exn(__FILE__,__LINE__,"iota: step must be > high - low"));
     }
     
     dimension = (lround (ceil (((high - low) / step) + 0.5)));
     
     assert (dimension > 0);
     
     k->size = dimension;
     
     assert ((k->data = malloc (dimension * sizeof (double))) != NULL);
     
     x = low;
     
     for (i = 0; i < dimension; i++)
	  
     {
	  argblock[2].Heapval = &ret;
	  argblock[1].RealNum = x;
	  argblock[0].Heapval = f->env;
	  
	  sm (f->code);
	  
	  k->data[i] = result.RealNum;
	  
	  x = x + step;
     }
     return l;
}
