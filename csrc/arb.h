#include <flint/arb.h>

// memory mgmnt
arb_struct* arb_new();
void arb_drop(arb_t t);
arb_struct *arb_vec_new( ulong n );
void arb_vec_drop(arb_ptr t, ulong n);

// macros
arf_struct* arb_midref_ (arb_t x);
mag_struct* arb_radref_( arb_t x );