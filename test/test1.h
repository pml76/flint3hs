

typedef enum
{
   FLINT_ERROR,     
   FLINT_OVERFLOW,  
   FLINT_IMPINV,    
   FLINT_DOMERR,    
   FLINT_DIVZERO,   
   FLINT_EXPOF,     
   FLINT_INEXACT,   
   FLINT_TEST_FAIL  
} flint_err_t2;


typedef int flint_err_t;

void flint_throw(flint_err_t exc, const char * msg );
flint_err_t2 flint_throw2(flint_err_t2 exc, const char * msg );