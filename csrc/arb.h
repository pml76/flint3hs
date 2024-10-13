
#ifndef __ARB__
#define __ARB__


#include <mpfr.h>
#include <flint/arb.h>

// memory mgmnt
arb_struct* arb_new();
void arb_drop(arb_t t);

// macros
arf_struct* arb_midref_ (arb_t x);
mag_struct* arb_radref_( arb_t x );


typedef struct {
    ulong n;
} arb_vec_env;

arb_vec_env *arb_vec_env_new( ulong n );
void arb_vec_env_drop( arb_vec_env* env); 
arb_struct *arb_vec_new( ulong n );
void arb_vec_drop(arb_vec_env* env, arb_ptr t);


#endif // __ARB__