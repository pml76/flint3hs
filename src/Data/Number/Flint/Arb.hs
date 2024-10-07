module Data.Number.Flint.Arb (

  -- * Types, macros and constants
    arb_midref
  , arb_radref

  -- * Memory management
  , arb_new
  , arb_drop
  , arb_vec_new
  , arb_vec_drop
  , arb_swap
  , arb_allocated_bytes
  , arb_vec_allocated_bytes
  , arb_vec_estimate_allocated_bytes

  -- * Assignment and rounding
  , ArbC
) where

import Data.Number.Flint.Arf ( ArfC )
import Data.Number.Flint.Mag ( MagC )
import Foreign.Ptr ( Ptr )
import Foreign.C.Types ( CULong(..), CLong(..) )


data ArbC

foreign import capi "arb.h arb_new" arb_new :: IO (Ptr ArbC)
-- ^ creates a new ArbC 

foreign import capi "arb.h arb_vec_new" arb_vec_new :: CULong -> IO (Ptr ArbC)
-- ^ creates a new vector of ArbC's of given length

foreign import capi "arb.h arb_vec_drop" arb_vec_drop :: Ptr ArbC -> CULong -> IO ()
-- ^ destroy a vector of  ArbC's of given length

foreign import capi "arb.h arb_drop" arb_drop :: Ptr ArbC -> IO ()
-- ^ destroys an ArbC

foreign import capi "arb.h arb_midref_" arb_midref :: Ptr ArbC -> IO (Ptr ArfC) 
-- ^ Macro returning a pointer to the midpoint of x as an Ptr ArfC.

foreign import capi "arb.h arb_radref_" arb_radref :: Ptr ArbC -> IO (Ptr MagC)
-- ^ Macro returning a pointer to the radius of x as a Ptr MagC.

foreign import capi "flint/arb.h arb_swap" arb_swap :: Ptr ArbC -- ^ x
                                                    -> Ptr ArbC -- ^ y
                                                    -> IO ()
-- ^ Swaps x and y efficiently.

foreign import capi "flint/arb.h arb_allocated_bytes" arb_allocated_bytes :: Ptr ArbC -> IO CLong
-- ^ Returns the total number of bytes heap-allocated internally by this object. The count excludes the size of the structure itself. 
-- Add sizeof(arb_struct) to get the size of the object as a whole.

foreign import capi "flint/arb.h _arb_vec_allocated_bytes" arb_vec_allocated_bytes :: Ptr ArbC -> CLong -> IO CLong 
-- ^ Returns the total number of bytes allocated for this vector, i.e. the space taken up by the vector itself plus the sum of the 
-- internal heap allocation sizes for all its member elements.

foreign import capi "flint/arb.h _arb_vec_estimate_allocated_bytes" arb_vec_estimate_allocated_bytes :: CLong -> CLong -> IO Double 
-- ^ Estimates the number of bytes that need to be allocated for a vector of len elements with prec bits of precision, including the 
-- space for internal limb data. This function returns a double to avoid overflow issues when both len and prec are large.

-- ^ This is only an approximation of the physical memory that will be used by an actual vector. In practice, the space varies 
-- with the content of the numbers; for example, zeros and small integers require no internal heap allocation even if the precision 
-- is huge. The estimate assumes that exponents will not be bignums. The actual amount may also be higher or lower due to overhead in 
-- the memory allocator or overcommitment by the operating system.


foreign import capi "flint/arb.h arb_set" arb_set :: Ptr ArbC  -- ^ y
                                                  -> Ptr ArbC  -- ^ x
                                                  -> IO (Ptr ArbC)
-- ^ copy the value of @x@ to @y@.

void arb_set_arf(arb_t y, const arf_t x)
void arb_set_si(arb_t y, slong x)
void arb_set_ui(arb_t y, ulong x)
void arb_set_fmpz(arb_t y, const fmpz_t x)
void arb_set_d(arb_t y, double x)
Sets y to the value of x without rounding.

Note Be cautious when using arb_set_d() as it does not impose any error bounds and will only convert a double to an arb_t. For instance, arb_set_d(x, 1.1) and arb_set_str(x, "1.1", prec) work very differently, where the former will first create a double whose value is the approximation of 
 (without any error bounds) which then sets x to this approximated value with no error. This differs from arb_set_str which will impose an error bound based on the precision.
void arb_set_fmpz_2exp(arb_t y, const fmpz_t x, const fmpz_t e)
Sets y to 
.

void arb_set_round(arb_t y, const arb_t x, slong prec)
void arb_set_round_fmpz(arb_t y, const fmpz_t x, slong prec)
Sets y to the value of x, rounded to prec bits in the direction towards zero.

void arb_set_round_fmpz_2exp(arb_t y, const fmpz_t x, const fmpz_t e, slong prec)
Sets y to 
, rounded to prec bits in the direction towards zero.

void arb_set_fmpq(arb_t y, const fmpq_t x, slong prec)
Sets y to the rational number x, rounded to prec bits in the direction towards zero.

int arb_set_str(arb_t res, const char *inp, slong prec)
Sets res to the value specified by the human-readable string inp. The input may be a decimal floating-point literal, such as “25”, “0.001”, “7e+141” or “-31.4159e-1”, and may also consist of two such literals separated by the symbol “+/-” and optionally enclosed in brackets, e.g. “[3.25 +/- 0.0001]”, or simply “[+/- 10]” with an implicit zero midpoint. The output is rounded to prec bits, and if the binary-to-decimal conversion is inexact, the resulting error is added to the radius.

The symbols “inf” and “nan” are recognized (a nan midpoint results in an indeterminate interval, with infinite radius).

Returns 0 if successful and nonzero if unsuccessful. If unsuccessful, the result is set to an indeterminate interval.

char *arb_get_str(const arb_t x, slong n, ulong flags)
Returns a nice human-readable representation of x, with at most n digits of the midpoint printed.

With default flags, the output can be parsed back with arb_set_str(), and this is guaranteed to produce an interval containing the original interval x.

By default, the output is rounded so that the value given for the midpoint is correct up to 1 ulp (unit in the last decimal place).

If ARB_STR_MORE is added to flags, more (possibly incorrect) digits may be printed.

If ARB_STR_NO_RADIUS is added to flags, the radius is not included in the output. Unless ARB_STR_MORE is set, the output is rounded so that the midpoint is correct to 1 ulp. As a special case, if there are no significant digits after rounding, the result will be shown as 0e+n, meaning that the result is between -1e+n and 1e+n (following the contract that the output is correct to within one unit in the only shown digit).

By adding a multiple m of ARB_STR_CONDENSE to flags, strings of more than three times m consecutive digits are condensed, only printing the leading and trailing m digits along with brackets indicating the number of digits omitted (useful when computing values to extremely high precision).