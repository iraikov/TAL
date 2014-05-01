#ifndef SUPPORT_H
#define SUPPORT_H

typedef unsigned int code_address_t;
typedef void *heap_address_t;

typedef union
{
  int IntNum;
  double RealNum;
  unsigned char BoolVal;
  code_address_t Procedure;
  heap_address_t Heapval;
} value_t;

#define TYPE_INT  0
#define TYPE_REAL 1
#define TYPE_BOOL 2
#define TYPE_PROC 3
#define TYPE_HEAP 4

/* this structure is used to call functions in the state machine
   - field code is the address of the function to be called 
   - field env is the environment of the function to be called 
*/

typedef struct
{
  code_address_t code;
  heap_address_t env;
} closure_t;


typedef struct str_internal 
{
     size_t size; 
     char *chars;
} 
*string_t;

typedef struct arr_internal 
{  
     size_t size; 
     void *elts;
}
*array_t;

typedef struct exn_internal 
{ 
     string_t loc_str;
     void *arg;
}
*exn_t;

/* exception & error reporting */

#define ENOENT   2
#define EINTR    4
#define ECHILD  10
#define EAGAIN  11
#define EACCES  13
#define ENOTDIR 20
#define EINVAL  22
#define EPIPE   32


void  clib_raise_exn (exn_t exn);
exn_t clib_make_exn  (char *file, int line, char *msg);
exn_t clib_make_nullpointer_exn (char *file, int line);

/* strings */
char *clib_string_to_Cstring(string_t);
string_t clib_Cstring_to_string(char *);    


/* utilities */
#define CLIB_HEAP_PAIR_T    1
#define CLIB_HEAP_STR_T     2

void *clib_malloc(int typ);
void clib_free (void *ptr, int typ);

int  clib_convert_flags(array_t flags, int flag_table[]);
array_t clib_copy_array(array_t result, int nbr, void *(*f)(char *), char ** arr);


#endif
