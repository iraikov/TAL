#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include <clib/support.h>

/**************/
/* Allocation */
/**************/

void out_of_memory(char *fn) 
{
     fprintf (stderr, "CLIB: Out of memory in function %s\n", fn);
     abort();
}

#define MAX_S_STRING 512
#define MAX_N_STRING 32

typedef struct 
{
     char available;
     char data [MAX_S_STRING+1];
     struct str_internal string;

} string_heap_elm_t;

string_heap_elm_t string_heap [MAX_N_STRING];

#define MAX_N_PAIR 32

typedef struct 
{
     char available;
     void *pair[2];

} pair_heap_elm_t;

pair_heap_elm_t pair_heap [MAX_N_PAIR];


void *clib_malloc(int typ) 
{
     int i;
     char available = 0;
     void *ptr = NULL;

     switch (typ)
     {
     case CLIB_HEAP_STR_T: 
     {
	  string_heap_elm_t *elm;

	  for (i=0; i<MAX_N_STRING; i++)
	  {
	       elm = &(string_heap[i]);
	       available = elm->available;
	       if (available > 0)
	       {
		    elm->available = 0;
		    elm->string.size = MAX_S_STRING;
		    elm->string.chars = elm->data;
		    memset(elm->data, 0, MAX_S_STRING+1);
		    ptr = (void *)&(elm->string);
		    break;
	       }
	  }
     }; break;

     case CLIB_HEAP_PAIR_T: 
     {
	  pair_heap_elm_t *elm;

	  for (i=0; i<MAX_N_PAIR; i++)
	  {
	       elm = &(pair_heap[i]);
	       available = elm->available;
	       if (available > 0)
	       {
		    elm->available = 0;
		    memset(elm->pair, 0, 2*(sizeof(void*)));
		    ptr = (void *)(elm->pair);
		    break;
	       }
	  }
     }; break;

     default:
     {
	  fprintf (stderr, "CLIB: unknown heap type %d\n", typ);
	  abort();

     }; break;
     }

     if (!(available > 0)) 
	  out_of_memory("clib_malloc");


     return ptr;
}


void clib_free (void *ptr, int typ)
{
     int i;

     switch (typ)
     {
     case CLIB_HEAP_STR_T: 
     {
	  string_heap_elm_t *elm;

	  for (i=0; i<MAX_N_STRING; i++)
	  {
	       elm = &(string_heap[i]);
	       if (ptr == (void *)&(elm->string))
	       {
		    elm->available = 1;
		    break;
	       }
	  }
     }; break;

     case CLIB_HEAP_PAIR_T: 
     {
	  pair_heap_elm_t *elm;

	  for (i=0; i<MAX_N_STRING; i++)
	  {
	       elm = &(pair_heap[i]);
	       if (ptr == (void *)(elm->pair))
	       {
		    elm->available = 1;
		    break;
	       }
	  }
     }; break;

     default:
     {
	  fprintf (stderr, "CLIB: unknown heap type %d\n", typ);
	  abort();

     }; break;
     }

     if (ptr == NULL) 
     {
	  fprintf (stderr, "CLIB: unknown pointer\n");
	  abort();
     }
}


/***************************/
/* Converting to and from C strings */
/***************************/

char *clib_string_to_Cstring (string_t s)
{
     int i;

     for (i=0; i<s->size; i++) 
     {
	  if (s->chars[i] == '\0') 
	       return s->chars;
     }

     if (s->size <= MAX_S_STRING)
     {
	  s->chars[s->size] = '\0';
	  return s->chars;
     }

     fprintf (stderr, "CLIB: string too long\n");
     abort();
}

string_t clib_Cstring_to_string (char *s)
{
     string_t result;
     int slen, rlen;

     if (s == NULL) 
     {
	  fprintf (stderr, "CLIB: null string\n");
	  abort();
     }

     slen = strlen (s);

     result = clib_malloc (CLIB_HEAP_STR_T);
     if (slen > MAX_S_STRING)
	  rlen = MAX_S_STRING;
     else
	  rlen = slen;

     result->size = rlen;
     strncpy(result->chars, s, rlen);

     return result;
}



array_t clib_copy_array(array_t result, int nbr, void *(*f)(char *), char ** arr) 
{
     int i;

     result->size = nbr;
     for (i = 0; i < nbr; i++) 
     {
	  ((void **)(result->elts))[i] = f(arr[i]);
     }

     return result;
}

/* Converts an array of flags (a union type) to a single
   integer using bit tricks */
int clib_convert_flags(array_t flags, int flag_table[]) 
{
     int i;
     int result = 0;
     for (i=0; i<flags->size; i++) 
     {
	  result |= flag_table[((int *)flags->elts)[i]-1];
     }
     return result;
}

/*******************/
/* Error reporting */
/*******************/

struct clib_exn_arg 
{
     int no;
     string_t syscall;
};

#define MAX_LOC_STR_LEN 100

struct exn_internal clib_Error_pkt;
struct exn_internal clib_NullPointer_pkt;

void clib_raise_exn (exn_t exn)
{
     fprintf(stderr, "CLIB: exception in %s: %s\n", exn->loc_str->chars,
	     ((struct clib_exn_arg *)(clib_Error_pkt.arg))->syscall->chars);
     abort();
}

void clib_format_exn(exn_t exn, char *file, int line) 
{
     string_t loc_buf;
     
     loc_buf = clib_malloc(CLIB_HEAP_STR_T);
     snprintf(loc_buf->chars, MAX_LOC_STR_LEN, "%s:%d", file, line);
     loc_buf->size = strlen (loc_buf->chars);

     exn->loc_str = loc_buf;
}

exn_t clib_make_exn(char *file, int line, char *msg) 
{
     clib_format_exn(&clib_Error_pkt,file,line);

     ((struct clib_exn_arg *)(clib_Error_pkt.arg))->no = errno; 
     ((struct clib_exn_arg *)(clib_Error_pkt.arg))->syscall->size = strlen(msg);
     ((struct clib_exn_arg *)(clib_Error_pkt.arg))->syscall->chars = msg;

     return &clib_Error_pkt;
}

exn_t clib_make_nullpointer_exn(char *file, int line) 
{
     clib_format_exn(&clib_NullPointer_pkt,file,line);
     clib_NullPointer_pkt.arg = NULL;

     return &clib_NullPointer_pkt;
}


string_t clib_error_string(int errno) 
{
     return (clib_Cstring_to_string(strerror(errno)));
}

