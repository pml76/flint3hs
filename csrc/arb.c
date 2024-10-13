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

arb_vec_env *arb_vec_env_new( ulong n ) {
    arb_vec_env *env = (arb_vec_env*) flint_malloc(sizeof(arb_vec_env));
    env->n = n;
    return env;
}


void arb_vec_env_drop( arb_vec_env* env) {
    flint_free(env);
}


void arb_vec_drop(arb_vec_env * env, arb_ptr t) {
    _arb_vec_clear(t, env->n);
    arb_vec_env_drop(env);
}




arf_struct* arb_midref_ (arb_t x) {
    return arb_midref(x); 
}

mag_struct* arb_radref_( arb_t x ) {
    return arb_radref(x); 
}


