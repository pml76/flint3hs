module Data.Number.Flint.Arb (
  -- * ArbC 
    ArbC
  , ArbVC
  
  -- * Types, macros and constants
  , arb_midref
  , arb_radref
  , arb_str_more
  , arb_str_no_radius
  , arb_str_condense

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
  , arb_set
  , arb_set_arf
  , arb_set_si
  , arb_set_ui
  , arb_set_fmpz
  , arb_set_fmpq
  , arb_set_d
  , arb_set_fmpz_2exp
  , arb_set_round
  , arb_set_round_fmpz
  , arb_set_round_fmpz_2exp
  , arb_set_str
  , arb_get_str

  -- * Assignment of special values
  , arb_zero
  , arb_one
  , arb_neg_inf
  , arb_pos_inf
  , arb_zero_pm_inf
  , arb_indeterminate
  , arb_zero_pm_one
  , arb_unit_interval

  -- * Input and output
  , arb_print
  , arb_printd
  , arb_printn
  , arb_dump_str
  , arb_load_str

  -- * Random number generation
  , arb_randtest
  , arb_randtest_exact
  , arb_randtest_positive
  , arb_randtest_precise
  , arb_randtest_special
  , arb_randtest_wide
  , arb_get_rand_fmpq
  , arb_urandom

  -- * Radius and interval operations

) where

import Data.Number.Flint.Arf ( ArfC )
import Data.Number.Flint.Mag ( MagC )
import Data.Number.Flint.Fmpz( FmpzC )
import Data.Number.Flint.Fmpq( FmpqC )
import Data.Number.Flint.Flint ( FlintRandC )
import Foreign.Ptr ( Ptr )
import Foreign.C.Types ( CULong(..), CLong(..), CInt(..), CDouble(..) )
import Foreign.C.String ( CString )


data ArbC
-- ^ An @Ptr ArbC@ (an @arb_t@ in arb-lang) represents a ball over the real numbers, that is, an interval \([m-r,m+m]\) where the midpoint \(m\) and 
-- the radius \(r\) are (extended) real numbers and \(r\) is nonnegative (possibly infinite). The result of an (approximate) 
-- operation done on @Ptr ArbC@ variables is a ball which contains the result of the (mathematically exact) operation applied 
-- to any choice of points in the input balls. In general, the output ball is not the smallest possible.
--
-- The precision parameter passed to each function roughly indicates the precision to which calculations on the midpoint are 
-- carried out (operations on the radius are always done using a fixed, small precision.)
--
-- For arithmetic operations, the precision parameter currently simply specifies the precision of the corresponding @Ptr ArbC@ 
-- operation. In the future, the arithmetic might be made faster by incorporating sloppy rounding (typically equivalent to a loss 
-- of 1-2 bits of effective working precision) when the result is known to be inexact (while still propagating errors rigorously, 
-- of course). Arithmetic operations done on exact input with exactly representable output are always guaranteed to produce exact 
-- output.
--
-- For more complex operations, the precision parameter indicates a minimum working precision (algorithms might allocate extra 
-- internal precision to attempt to produce an output accurate to the requested number of bits, especially when the required 
-- precision can be estimated easily, but this is not generally required).
--
-- If the precision is increased and the inputs either are exact or are computed with increased accuracy as well, the output 
-- should converge proportionally, absent any bugs. The general intended strategy for using ball arithmetic is to add a few guard 
-- bits, and then repeat the calculation as necessary with an exponentially increasing number of guard bits (Ziv’s strategy) until 
-- the result is exact enough for one’s purposes (typically the first attempt will be successful).
--
-- The following balls with an infinite or NaN component are permitted, and may be returned as output from functions.
--
-- - The ball \([+\infty \pm c]\), where \(c\) is finite, represents the point at positive infinity. Such a ball can always be replaced 
--   by \([+\infty \pm 0]\) while preserving mathematical correctness (this is currently not done automatically by the library).
-- - The ball \([-\infty \pm c]\), where \(c\) is finite, represents the point at negative infinity. Such a ball can always be replaced 
--   by \([-\infty \pm 0]\) while preserving mathematical correctness (this is currently not done automatically by the library).
-- - The ball \([c \pm \infty]\), where \(c\) is finite or infinite, represents the whole extended real line \([-\infty,+\infty]\). Such a 
--   ball can always be replaced by \([0 \pm \infty]\) while preserving mathematical correctness (this is currently not done automatically 
--   by the library). Note that there is no way to represent a half-infinite interval such as \([0,\infty]\).
-- - The ball \([\operatorname{NaN} \pm c]\), where \(c\) is finite or infinite, represents an indeterminate value (the value could be any 
--   extended real number, or it could represent a function being evaluated outside its domain of definition, for example where the result 
--   would be complex). Such an indeterminate ball can always be replaced by \([\operatorname{NaN} \pm \infty]\) while preserving 
--   mathematical correctness (this is currently not done automatically by the library).

data ArbVC
-- ^ A @Ptr ArbVC@ represents a vector of @Ptr ArbC@'s. In can be imaginated as a @Ptr (Ptr ArbC)@, although is is not represented as a 
-- pointer of pointers internally. 

foreign import capi safe "arb.h arb_midref_" arb_midref :: Ptr ArbC -> IO (Ptr ArfC) 
-- ^ Macro returning a pointer to the midpoint of x as an Ptr ArfC.

foreign import capi safe "arb.h arb_radref_" arb_radref :: Ptr ArbC -> IO (Ptr MagC)
-- ^ Macro returning a pointer to the radius of x as a Ptr MagC.


foreign import capi safe "flint/arb.h value ARB_STR_MORE" arb_str_more :: CLong
-- ^ If @arb_str_more@ is added to @flags@ in a call of @arb_get_str@, more (possibly incorrect) digits may be printed.

foreign import capi safe "flint/arb.h value ARB_STR_NO_RADIUS" arb_str_no_radius :: CLong 
-- ^ If @arb_str_no_radius@ is added to @flags@ in a call of @arb_get_str@, the radius is not included in the output. 
-- Unless @arb_str_more@ is set, the output is rounded so that the midpoint is correct to 1 ulp. As a special case, 
-- if there are no significant digits after rounding, the result will be shown as @0e+n@, meaning that the result is between @-1e+n@ and @1e+n@ 
-- (following the contract that the output is correct to within one unit in the only shown digit).

foreign import capi safe "flint/arb.h value ARB_STR_CONDENSE" arb_str_condense :: CLong 
-- ^ By adding a multiple @m@ of ARB_STR_CONDENSE to flags, strings of more than three times @m@ consecutive digits are 
-- condensed, only printing the leading and trailing @m@ digits along with brackets indicating the number of digits omitted 
-- (useful when computing values to extremely high precision).


foreign import capi safe "arb.h arb_new" arb_new :: IO (Ptr ArbC)
-- ^ creates a new ArbC 

foreign import capi safe "arb.h arb_vec_new" arb_vec_new :: CULong -> IO (Ptr ArbVC)
-- ^ creates a new vector of ArbC's of given length

foreign import capi safe "arb.h arb_vec_drop" arb_vec_drop :: Ptr ArbVC -> CULong -> IO ()
-- ^ destroy a vector of  ArbC's of given length

foreign import capi safe "arb.h arb_drop" arb_drop :: Ptr ArbC -> IO ()
-- ^ destroys an ArbC

foreign import capi safe "flint/arb.h arb_swap" arb_swap :: Ptr ArbC -- ^ x
                                                    -> Ptr ArbC -- ^ y
                                                    -> IO ()
-- ^ Swaps x and y efficiently.

foreign import capi safe "flint/arb.h arb_allocated_bytes" arb_allocated_bytes :: Ptr ArbC -> IO CLong
-- ^ Returns the total number of bytes heap-allocated internally by this object. The count excludes the size of the structure itself. 
-- Add sizeof(arb_struct) to get the size of the object as a whole.

foreign import capi safe "flint/arb.h _arb_vec_allocated_bytes" arb_vec_allocated_bytes :: Ptr ArbC -> CLong -> IO CLong 
-- ^ Returns the total number of bytes allocated for this vector, i.e. the space taken up by the vector itself plus the sum of the 
-- internal heap allocation sizes for all its member elements.

foreign import capi safe "flint/arb.h _arb_vec_estimate_allocated_bytes" arb_vec_estimate_allocated_bytes :: CLong -> CLong -> IO Double 
-- ^ Estimates the number of bytes that need to be allocated for a vector of len elements with prec bits of precision, including the 
-- space for internal limb data. This function returns a double to avoid overflow issues when both len and prec are large.

-- ^ This is only an approximation of the physical memory that will be used by an actual vector. In practice, the space varies 
-- with the content of the numbers; for example, zeros and small integers require no internal heap allocation even if the precision 
-- is huge. The estimate assumes that exponents will not be bignums. The actual amount may also be higher or lower due to overhead in 
-- the memory allocator or overcommitment by the operating system.


foreign import capi safe "flint/arb.h arb_set" arb_set :: Ptr ArbC  -- ^ y
                                                       -> Ptr ArbC  -- ^ x
                                                       -> IO ()
-- ^ copy the value of @x@ to @y@ without rounding.

foreign import capi safe "flint/arb.h arb_set_arf" arb_set_arf :: Ptr ArbC  -- ^ y
                                                               -> Ptr ArfC  -- ^ x
                                                               -> IO () 
-- ^ copy the value of @x@ to @y@ without rounding.

foreign import capi safe "flint/arb.h arb_set_si" arb_set_si :: Ptr ArbC  -- ^ y
                                                             -> CLong     -- ^ x
                                                             -> IO ()
-- ^ copy the value of @x@ to @y@ without rounding.

foreign import capi safe "flint/arb.h arb_set_ui" arb_set_ui :: Ptr ArbC  -- ^ y
                                                             -> CULong    -- ^ x
                                                             -> IO ()
-- ^ copy the value of @x@ to @y@ without rounding.

foreign import capi safe "flint/arb.h arb_set_fmpz" arb_set_fmpz :: Ptr ArbC  -- ^ y
                                                                 -> Ptr FmpzC -- ^ x
                                                                 -> IO ()
-- ^ copy the value of @x@ to @y@ without rounding.

foreign import capi safe "flint/arb.h arb_set_d" arb_set_d :: Ptr ArbC  -- ^ y
                                                           -> CDouble   -- ^ x
                                                           -> IO ()
-- ^ copy the value of @x@ to @y@ without rounding.
-- 
-- Note Be cautious when using @arb_set_d@ as it does not impose any error bounds and will only convert a double to an @ArbC@. For instance, @ arb_set_d x 1.1@ and @arb_set_str x "1.1" prec@ 
-- work very differently, where the former will first create a double whose value is the approximation of @1.1@ (without any error bounds) which then sets @x@ to this approximated value 
-- with no error. This differs from @arb_set_str@ which will impose an error bound based on the precision.

foreign import capi safe "flint/arb.h arb_set_fmpz_2exp" arb_set_fmpz_2exp :: Ptr ArbC  -- ^ y 
                                                                           -> Ptr FmpzC -- ^ x
                                                                           -> Ptr FmpzC -- ^ e
                                                                           -> IO ()
-- ^ Sets @y@ to \(x2^e\).

foreign import capi safe "flint/arb.h arb_set_round" arb_set_round :: Ptr ArbC  -- ^ y
                                                     -> Ptr ArbC                -- ^ x
                                                     -> CLong                   -- ^ prec
                                                     -> IO ()
-- ^ Sets @y@ to the value of @x@, rounded to @prec@ bits in the direction towards zero.

foreign import capi safe "flint/arb.h arb_set_round_fmpz" arb_set_round_fmpz :: Ptr ArbC  -- ^ y
                                                                             -> Ptr FmpzC -- ^ x
                                                                             -> CLong     -- ^ prec
                                                                             -> IO ()
-- ^ Sets @y@ to the value of @x@, rounded to @prec@ bits in the direction towards zero.

foreign import capi safe "flint/arb.h arb_set_round_fmpz_2exp" arb_set_round_fmpz_2exp :: Ptr ArbC  -- ^ y
                                                                                       -> Ptr FmpzC -- ^ x
                                                                                       -> Ptr FmpzC -- ^ e
                                                                                       -> CLong     -- ^ prec 
                                                                                       -> IO ()
-- ^ Sets @y@ to \(x 2^e\), rounded to @prec@ bits in the direction towards zero.


foreign import capi safe "flint/arb.h arb_set_fmpq" arb_set_fmpq :: Ptr ArbC  -- ^ y
                                                                 -> Ptr FmpqC -- ^ x
                                                                 -> CLong     -- prec
                                                                 -> IO ()
-- ^ Sets @y @to the rational number @x@, rounded to @prec@ bits in the direction towards zero.

foreign import capi safe "flint/arb.h arb_set_str" arb_set_str :: Ptr ArbC  -- ^ res
                                                               -> Foreign.C.String.CString   -- ^ inp
                                                               -> CLong     -- ^ prec
                                                               -> IO CInt
-- ^ Sets @res@ to the value specified by the human-readable string @inp@. The input may be a 
-- decimal floating-point literal, such as “25”, “0.001”, “7e+141” or “-31.4159e-1”, and 
-- may also consist of two such literals separated by the symbol “+/-” and optionally enclosed 
-- in brackets, e.g. “[3.25 +/- 0.0001]”, or simply “[+/- 10]” with an implicit zero midpoint. 
-- The output is rounded to @prec@ bits, and if the binary-to-decimal conversion is inexact, 
-- the resulting error is added to the radius.
--
-- The symbols “inf” and “nan” are recognized (a nan midpoint results in an indeterminate interval, with infinite radius).
--
-- Returns 0 if successful and nonzero if unsuccessful. If unsuccessful, the result is set to an indeterminate interval.




foreign import capi safe "flint/arb.h arb_get_str" arb_get_str :: Ptr ArbC  -- ^ x
                                                               -> CLong     -- ^ n
                                                               -> CULong    -- ^ flags
                                                               -> IO Foreign.C.String.CString 
-- ^ Returns a nice human-readable representation of @x@, with at most @n@ digits of the midpoint printed.
--
-- With default flags, the output can be parsed back with @arb_set_str@, and this is guaranteed to produce 
-- an interval containing the original interval @x@.
--
-- By default, the output is rounded so that the value given for the midpoint is correct up to 1 ulp (unit in 
-- the last decimal place).
--
-- If @arb_str_more@ is added to @flags@ in a call of @arb_get_str@, more (possibly incorrect) digits may be printed.
--
-- If @arb_str_no_radius@ is added to @flags@ in a call of @arb_get_str@, the radius is not included in the output. 
-- Unless @arb_str_more@ is set, the output is rounded so that the midpoint is correct to 1 ulp. As a special case, 
-- if there are no significant digits after rounding, the result will be shown as @0e+n@, meaning that the result is between @-1e+n@ and @1e+n@ 
-- (following the contract that the output is correct to within one unit in the only shown digit).
--
-- By adding a multiple @m@ of ARB_STR_CONDENSE to flags, strings of more than three times @m@ consecutive digits are 
-- condensed, only printing the leading and trailing @m@ digits along with brackets indicating the number of digits omitted 
-- (useful when computing values to extremely high precision).



foreign import capi safe "flint/arb.h arb_zero" arb_zero :: Ptr ArbC -- ^ x
                                                         -> IO ()
-- ^ Sets @x@ to zero.

foreign import capi safe "flint/arb.h arb_one" arb_one :: Ptr ArbC -- ^ x
                                                       -> IO ()
-- ^ Sets @x@ to the exact integer @1@.

foreign import capi safe "flint/arb.h arb_pos_inf" arb_pos_inf :: Ptr ArbC -- ^ x
                                                               -> IO ()
-- ^ Sets @x@ to positive infinity, with a zero radius.

foreign import capi safe "flint/arb.h arb_neg_inf" arb_neg_inf :: Ptr ArbC -- ^ x
                                                               -> IO ()
-- ^ Sets @x@ to negative infinity, with a zero radius.

foreign import capi safe "flint/arb.h arb_zero_pm_inf" arb_zero_pm_inf :: Ptr ArbC -- ^ x
                                                                       -> IO ()
-- ^ Sets @x@ to \([0 \pm \infty]\), representing the whole extended real line.

foreign import capi safe "flint/arb.h arb_indeterminate" arb_indeterminate :: Ptr ArbC -- ^ x
                                                                           -> IO ()
-- ^ Sets @x@ to \([\operatorname{NaN} \pm \infty]\), representing an indeterminate result.

foreign import capi safe "flint/arb.h arb_zero_pm_one" arb_zero_pm_one :: Ptr ArbC -- ^ x
                                                                       -> IO ()
-- ^ Sets @x@ to the interval \([0 \pm 1]\).

foreign import capi safe "flint/arb.h arb_unit_interval" arb_unit_interval :: Ptr ArbC -- ^ x
                                                                           -> IO ()
-- ^ Sets @x@ to the interval \([0, 1]\).




-------------------------------------------------------------------------------

foreign import capi safe "flint/arb.h arb_print" arb_print :: Ptr ArbC -- ^ x
                                                           -> IO ()
-- ^ Prints the internal representation of @x@.

foreign import capi safe "flint/arb.h arb_printd" arb_printd :: Ptr ArbC -- ^ x
                                                             -> CLong    -- ^ d
                                                             -> IO ()
-- ^ Prints @x@ in decimal. The printed value of the radius is not adjusted
-- to compensate for the fact that the binary-to-decimal conversion
-- of both the midpoint and the radius introduces additional error.


foreign import capi safe "flint/arb.h arb_printn" arb_printn :: Ptr ArbC -- ^ x
                                                             -> CLong    -- ^ d
                                                             -> CLong    -- ^ f
                                                             -> IO ()
-- ^ Prints a nice decimal representation of @x@.
-- By default, the output shows the midpoint with a guaranteed error of at
-- most one unit in the last decimal place. In addition, an explicit error
-- bound is printed so that the displayed decimal interval is guaranteed to
-- enclose @x@.
--
-- See @arb_get_str@ for details.

foreign import capi safe "flint/arb.h arb_dump_str" arb_dump_str :: Ptr ArbC -- ^ x
                                                                 -> IO CString
-- ^ Returns a serialized representation of @x@ as a null-terminated
-- ASCII string that can be read by @arb_load_str@. The format consists
-- of four hexadecimal integers representing the midpoint mantissa,
-- midpoint exponent, radius mantissa, and radius exponent (with special
-- values to indicate zero, infinity and NaN values),
-- separated by single spaces. The returned string needs to be deallocated
-- with @flint_free@.

foreign import capi safe "flint/arb.h" arb_load_str :: Ptr ArbC -- ^ x
                                                    -> CString -- ^ str
                                                    -> IO CInt
-- ^ Sets @x@ to the serialized representation given in @str@. Returns a
-- nonzero value if @str@ is not formatted correctly (see :@arb_dump_str@).



 
-------------------------------------------------------------------------------

foreign import capi safe "flint/arb.h arb_randtest" arb_randtest :: Ptr ArbC         -- ^ x
                                                                -> Ptr FlintRandC   -- ^ state
                                                                -> CLong            -- ^ prec
                                                                -> CLong            -- ^ mag_bits
                                                                -> IO ()
-- ^ Generates a random ball. The midpoint and radius will both be finite.

foreign import capi safe "flint/arb.h arb_randtest_exact" arb_randtest_exact :: Ptr ArbC        -- ^ x
                                                                             -> Ptr FlintRandC  -- ^ state
                                                                             -> CLong           -- ^ prec
                                                                             -> CLong           -- ^ mag_bits
                                                                             -> IO ()
-- ^ Generates a random number with zero radius.

foreign import capi safe "flint/arb.h arb_randtest_precise" arb_randtest_precise :: Ptr ArbC        -- ^ x
                                                                                 -> Ptr FlintRandC  -- ^ state
                                                                                 -> CLong           -- ^ prec
                                                                                 -> CLong           -- ^ mag_bits
                                                                                 -> IO ()
-- ^ Generates a random number with radius around \(2^{-\text{prec}}\) the magnitude of the midpoint.


foreign import capi safe "flint/arb.h arb_randtest_positive" arb_randtest_positive :: Ptr ArbC        -- ^ x
                                                                                   -> Ptr FlintRandC  -- ^ state
                                                                                   -> CLong           -- ^ prec
                                                                                   -> CLong           -- ^ mag_bits
                                                                                   -> IO ()
-- ^ Generates a random precise number which is guaranteed to be positive.

foreign import capi safe "flint/arb.h arb_randtest_wide" arb_randtest_wide :: Ptr ArbC        -- ^ x
                                                                           -> Ptr FlintRandC  -- ^ state
                                                                           -> CLong           -- ^ prec
                                                                           -> CLong           -- ^ mag_bits
                                                                           -> IO ()
-- ^ Generates a random number with midpoint and radius chosen independently,
-- possibly giving a very large interval.


foreign import capi safe "flint/arb.h arb_randtest_special" arb_randtest_special :: Ptr ArbC        -- ^ x
                                                                                 -> Ptr FlintRandC  -- ^ state
                                                                                 -> CLong           -- ^ prec
                                                                                 -> CLong           -- ^ mag_bits
                                                                                 -> IO ()
-- ^ Generates a random interval, possibly having NaN or an infinity as the midpoint 
-- and possibly having an infinite radius.


foreign import capi safe "flint/arb.h arb_get_rand_fmpq" arb_get_rand_fmpq :: Ptr FmpqC       -- ^ q
                                                                           -> Ptr FlintRandC  -- ^ state
                                                                           -> Ptr ArbC        -- ^ x
                                                                           -> CLong           -- ^ bits
                                                                           -> IO ()
-- ^ Sets @q@ to a random rational number from the interval represented by @x@.
-- A denominator is chosen by multiplying the binary denominator of @x@
-- by a random integer up to @bits@ bits.
--
-- The outcome is undefined if the midpoint or radius of @x@ is non-finite,
-- or if the exponent of the midpoint or radius is so large or small
-- that representing the endpoints as exact rational numbers would
-- cause overflows.

foreign import capi safe "flint/arb.h arb_urandom" arb_urandom :: Ptr ArbC        -- ^ x
                                                               -> Ptr FlintRandC  -- ^ state
                                                               -> CLong           -- ^ prec
                                                               -> IO ()
-- ^ Sets @x@ to a uniformly distributed random number in the interval
-- \([0, 1]\). The method uses rounding from integers to floats, hence the
-- radius might not be \(0\).




-------------------------------------------------------------------------------

foreign import capi safe "flint/arb.h arb_get_mid_arb" arb_get_mid_arb :: Ptr ArbC -- ^ m
                                                                       -> Ptr ArbC -- ^ x
-- ^ Sets @m@ to the midpoint of @x@.

.. function:: void arb_get_rad_arb(arb_t r, const arb_t x)

    Sets *r* to the radius of *x*.

.. function:: void arb_add_error_arf(arb_t x, const arf_t err)

.. function:: void arb_add_error_mag(arb_t x, const mag_t err)

.. function:: void arb_add_error(arb_t x, const arb_t err)

    Adds the absolute value of *err* to the radius of *x* (the operation
    is done in-place).

.. function:: void arb_add_error_2exp_si(arb_t x, slong e)

.. function:: void arb_add_error_2exp_fmpz(arb_t x, const fmpz_t e)

    Adds `2^e` to the radius of *x*.

.. function:: void arb_union(arb_t z, const arb_t x, const arb_t y, slong prec)

    Sets *z* to a ball containing both *x* and *y*.

.. function:: int arb_intersection(arb_t z, const arb_t x, const arb_t y, slong prec)

    If *x* and *y* overlap according to :func:`arb_overlaps`,
    then *z* is set to a ball containing the intersection of *x* and *y*
    and a nonzero value is returned.
    Otherwise zero is returned and the value of *z* is undefined.
    If *x* or *y* contains NaN, the result is NaN.

.. function:: void arb_nonnegative_part(arb_t res, const arb_t x)

    Sets *res* to the intersection of *x* with `[0,\infty]`. If *x* is
    nonnegative, an exact copy is made. If *x* is finite and contains negative
    numbers, an interval of the form `[r/2 \pm r/2]` is produced, which
    certainly contains no negative points.
    In the special case when *x* is strictly negative, *res* is set to zero.

.. function:: void arb_get_abs_ubound_arf(arf_t u, const arb_t x, slong prec)

    Sets *u* to the upper bound for the absolute value of *x*,
    rounded up to *prec* bits. If *x* contains NaN, the result is NaN.

.. function:: void arb_get_abs_lbound_arf(arf_t u, const arb_t x, slong prec)

    Sets *u* to the lower bound for the absolute value of *x*,
    rounded down to *prec* bits. If *x* contains NaN, the result is NaN.

.. function:: void arb_get_ubound_arf(arf_t u, const arb_t x, slong prec)

    Sets *u* to the upper bound for the value of *x*,
    rounded up to *prec* bits. If *x* contains NaN, the result is NaN.

.. function:: void arb_get_lbound_arf(arf_t u, const arb_t x, slong prec)

    Sets *u* to the lower bound for the value of *x*,
    rounded down to *prec* bits. If *x* contains NaN, the result is NaN.

.. function:: void arb_get_mag(mag_t z, const arb_t x)

    Sets *z* to an upper bound for the absolute value of *x*. If *x* contains
    NaN, the result is positive infinity.

.. function:: void arb_get_mag_lower(mag_t z, const arb_t x)

    Sets *z* to a lower bound for the absolute value of *x*. If *x* contains
    NaN, the result is zero.

.. function:: void arb_get_mag_lower_nonnegative(mag_t z, const arb_t x)

    Sets *z* to a lower bound for the signed value of *x*, or zero
    if *x* overlaps with the negative half-axis. If *x* contains NaN,
    the result is zero.

.. function:: void arb_get_interval_fmpz_2exp(fmpz_t a, fmpz_t b, fmpz_t exp, const arb_t x)

    Computes the exact interval represented by *x*, in the form of an integer
    interval multiplied by a power of two, i.e. `x = [a, b] \times 2^{\text{exp}}`.
    The result is normalized by removing common trailing zeros
    from *a* and *b*.

    This method aborts if *x* is infinite or NaN, or if the difference between
    the exponents of the midpoint and the radius is so large that allocating
    memory for the result fails.

    Warning: this method will allocate a huge amount of memory to store
    the result if the exponent difference is huge. Memory allocation could
    succeed even if the required space is far larger than the physical
    memory available on the machine, resulting in swapping. It is recommended
    to check that the midpoint and radius of *x* both are within a
    reasonable range before calling this method.

.. function:: void arb_set_interval_mag(arb_t x, const mag_t a, const mag_t b, slong prec)

.. function:: void arb_set_interval_arf(arb_t x, const arf_t a, const arf_t b, slong prec)

.. function:: void arb_set_interval_mpfr(arb_t x, const mpfr_t a, const mpfr_t b, slong prec)

    Sets *x* to a ball containing the interval `[a, b]`. We
    require that `a \le b`.

.. function:: void arb_set_interval_neg_pos_mag(arb_t x, const mag_t a, const mag_t b, slong prec)

    Sets *x* to a ball containing the interval `[-a, b]`.

.. function:: void arb_get_interval_arf(arf_t a, arf_t b, const arb_t x, slong prec)

.. function:: void arb_get_interval_mpfr(mpfr_t a, mpfr_t b, const arb_t x)

    Constructs an interval `[a, b]` containing the ball *x*. The MPFR version
    uses the precision of the output variables.

.. function:: slong arb_rel_error_bits(const arb_t x)

    Returns the effective relative error of *x* measured in bits, defined as
    the difference between the position of the top bit in the radius
    and the top bit in the midpoint, plus one.
    The result is clamped between plus/minus *ARF_PREC_EXACT*.

.. function:: slong arb_rel_accuracy_bits(const arb_t x)

    Returns the effective relative accuracy of *x* measured in bits,
    equal to the negative of the return value from :func:`arb_rel_error_bits`.

.. function:: slong arb_rel_one_accuracy_bits(const arb_t x)

    Given a ball with midpoint *m* and radius *r*, returns an approximation of
    the relative accuracy of `[\max(1,|m|) \pm r]` measured in bits.

.. function:: slong arb_bits(const arb_t x)

    Returns the number of bits needed to represent the absolute value
    of the mantissa of the midpoint of *x*, i.e. the minimum precision
    sufficient to represent *x* exactly. Returns 0 if the midpoint
    of *x* is a special value.

.. function:: void arb_trim(arb_t y, const arb_t x)

    Sets *y* to a trimmed copy of *x*: rounds *x* to a number of bits
    equal to the accuracy of *x* (as indicated by its radius),
    plus a few guard bits. The resulting ball is guaranteed to
    contain *x*, but is more economical if *x* has
    less than full accuracy.

.. function:: int arb_get_unique_fmpz(fmpz_t z, const arb_t x)

    If *x* contains a unique integer, sets *z* to that value and returns
    nonzero. Otherwise (if *x* represents no integers or more than one integer),
    returns zero.

    This method aborts if there is a unique integer but that integer
    is so large that allocating memory for the result fails.

    Warning: this method will allocate a huge amount of memory to store
    the result if there is a unique integer and that integer is huge.
    Memory allocation could succeed even if the required space is far
    larger than the physical memory available on the machine, resulting
    in swapping. It is recommended to check that the midpoint of *x* is
    within a reasonable range before calling this method.

.. function:: void arb_floor(arb_t y, const arb_t x, slong prec)
              void arb_ceil(arb_t y, const arb_t x, slong prec)
              void arb_trunc(arb_t y, const arb_t x, slong prec)
              void arb_nint(arb_t y, const arb_t x, slong prec)

    Sets *y* to a ball containing respectively, `\lfloor x \rfloor` and
    `\lceil x \rceil`, `\operatorname{trunc}(x)`, `\operatorname{nint}(x)`,
    with the midpoint of *y* rounded to at most *prec* bits.

.. function:: void arb_get_fmpz_mid_rad_10exp(fmpz_t mid, fmpz_t rad, fmpz_t exp, const arb_t x, slong n)

    Assuming that *x* is finite and not exactly zero, computes integers *mid*,
    *rad*, *exp* such that `x \in [m-r, m+r] \times 10^e` and such that the
    larger out of *mid* and *rad* has at least *n* digits plus a few guard
    digits. If *x* is infinite or exactly zero, the outputs are all set
    to zero.

.. function:: int arb_can_round_arf(const arb_t x, slong prec, arf_rnd_t rnd)

.. function:: int arb_can_round_mpfr(const arb_t x, slong prec, mpfr_rnd_t rnd)

    Returns nonzero if rounding the midpoint of *x* to *prec* bits in
    the direction *rnd* is guaranteed to give the unique correctly
    rounded floating-point approximation for the real number represented by *x*.

    In other words, if this function returns nonzero, applying
    :func:`arf_set_round`, or :func:`arf_get_mpfr`, or :func:`arf_get_d`
    to the midpoint of *x* is guaranteed to return a correctly rounded *arf_t*,
    *mpfr_t* (provided that *prec* is the precision of the output variable),
    or *double* (provided that *prec* is 53).
    Moreover, :func:`arf_get_mpfr` is guaranteed to return the correct ternary
    value according to MPFR semantics.

    Note that the *mpfr* version of this function takes an MPFR rounding mode
    symbol as input, while the *arf* version takes an *arf* rounding mode
    symbol. Otherwise, the functions are identical.

    This function may perform a fast, inexact test; that is, it may return
    zero in some cases even when correct rounding actually is possible.

    To be conservative, zero is returned when *x* is non-finite, even if it
    is an "exact" infinity.

    -}