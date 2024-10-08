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


) where

import Data.Number.Flint.Arf ( ArfC )
import Data.Number.Flint.Mag ( MagC )
import Data.Number.Flint.Fmpz( FmpzC )
import Data.Number.Flint.Fmpq( FmpqC )
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