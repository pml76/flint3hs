typedef enum
{
   FLINT_ERROR,     /* general error */
   FLINT_OVERFLOW,  /* overflow */
   FLINT_IMPINV,    /* impossible inverse */
   FLINT_DOMERR,    /* domain error */
   FLINT_DIVZERO,   /* divide by zero */
   FLINT_EXPOF,     /* exponent overflow */
   FLINT_INEXACT,   /* inexact error */
   FLINT_TEST_FAIL  /* test fail */
} flint_err_t;

void flint_throw(flint_err_t exc, const char * msg );