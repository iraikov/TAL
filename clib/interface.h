

#ifndef _INTERFACE_H
#define _INTERFACE_H


/* register block used for passing arguments between functions  */
extern value_t *argblock;

/* special return closure  */
extern closure_t ret;

/* result register  */
extern value_t result;

/* sm is the state machine function */
extern void sm (code_address_t x);



#endif /* _INTERFACE_H */
