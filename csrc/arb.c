#include <flint/arb.h>
#include "arb.h"

arb_struct *arb_new() {
    return _arb_vec_init(1);
}


void arb_drop(arb_t t) {
    _arb_vec_clear(t, 1);
}


arb_struct *arb_vec_new( ulong n ) {
    return _arb_vec_init(n);
}


void arb_vec_drop(arb_ptr t, ulong n) {
    _arb_vec_clear(t, n);
}


arf_struct* arb_midref_ (arb_t x) {
    return arb_midref(x); 
}

mag_struct* arb_radref_( arb_t x ) {
    return arb_radref(x); 
}


