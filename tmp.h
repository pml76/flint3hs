#ifndef __TMP_H
#define __TMP_H

typedef struct { int k; } myType; 
typedef myType myT[1];  

#include <arb.h>

int hello(long *k, myT l);

#endif // __TMP_H