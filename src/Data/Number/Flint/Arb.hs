{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Number.Flint.Arb (
  -- * Types, macros and constants
  Arb (..),
  ArbVector (..),
  arbCreate,
  arbCreateVector,
  arbMidRef,
  arbRadRef,
  arbStrMore,
  arbStrNoRadius,
  arbStrCondense,

  -- * Memory management
  arbSwap,
  arbAllocatedBytes,
  arbVecAllocatedBytes,
  arbVecEstimateAllocatedBytes,

  -- * Assignment and rounding
  arbSet,
  arbSetArf,
  arbSetSI,
  arbSetUI,
  arbSetFmpz,
  arbSetFmpq,
  arbSetD,
  arbSetFmpz2Exp,
  arbSetRound,
  arbSetRoundFmpz,
  arbSetRoundFmpz2Exp,
  arbSetStr,
  arbGetStr,

  -- * Assignment of special values
  arbZero,
  arbOne,
  arbNegInf,
  arbPosInf,
  arbZeroPMInf,
  arbIntermediate,
  arbZeroPMOne,
  arbUnitInterval,

  -- * Input and output
  arbPrint,
  arbPrintD,
  arbPrintN,
  arbDumpStr,
  arbLoadStr,

  -- * Random number generation
  arbRandtest,
  arbRandtestExact,
  arbRandtestPositive,
  arbRandtestPrecise,
  arbRandtestSpecial,
  arbRandtestWide,
  arbGetRandFmpq,
  arbUrandom,

  -- * Radius and interval operations
  arbGetMidArb,
  arbGetRadArb,
  arbAddError,
  arbAddError2ExpFmpz,
  arbAddError2ExpSI,
  arbAddErrorArf,
  arbAddErrorMag,
  arbUnion,
  arbIntersection,
  arbNonnegativePart,
  arbGetAbsLboundArf,
  arbGetAbsUboundArf,
  arbGetLboundArf,
  arbGetUboundArf,
  arbGetMag,
  arbGetMagLower,
  arbGetMagLowerNonnegative,
  arbGetIntervalFmpz2Exp,
  arbSetIntervalMag,
  arbSetIntervalArf,
  arbSetIntervalMpfr,
  arbSetIntervalNegPosMag,
  arbGetIntervalArf,
  arbGetIntervalMpfr,
  arbRefErrorBits,
  arbRelEAccuracyBits,
  arbRelOneEAccuracyBits,
  arbBits,
  arbTrim,
  arbGetUniqueFmpz,
  arbFloor,
  arbCeil,
  arbTrunc,
  arbNint,
  arbGetFmpzMidRad10Exp,
  arbCanRoundArf,
  arbCanRoundMpfr,
) where

import Data.Number.Flint.Arf (ArfRndC (..), ArfT (..))
import Data.Number.Flint.Flint (FlintRandT (..))
import Data.Number.Flint.FlintVariable
import Data.Number.Flint.Fmpq (FmpqT (..))
import Data.Number.Flint.Fmpz (FmpzT (..))
import Data.Number.Flint.Mag (MagT (..))
import Data.Number.Mpfr.Mpfr (MpfrRndC (..), MpfrT (..))
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble (..), CInt (..), CLong (..), CULong (..))
import Foreign.ForeignPtr (
  ForeignPtr,
  newForeignPtr,
  newForeignPtrEnv,
  withForeignPtr,
 )
import Foreign.Ptr (FunPtr, Ptr, nullPtr)

data ArbC

newtype ArbT where
  ArbT :: {_ArbT :: Ptr ArbC} -> ArbT
{- ^ An @ArbT@ (an @arb_t@ in arb-lang) represents a ball over the real numbers, that is, an interval \([m-r,m+m]\) where the midpoint \(m\) and
the radius \(r\) are (extended) real numbers and \(r\) is nonnegative (possibly infinite). The result of an (approximate)
operation done on @ArbT@ variables is a ball which contains the result of the (mathematically exact) operation applied
to any choice of points in the input balls. In general, the output ball is not the smallest possible.

The precision parameter passed to each function roughly indicates the precision to which calculations on the midpoint are
carried out (operations on the radius are always done using a fixed, small precision.)

For arithmetic operations, the precision parameter currently simply specifies the precision of the corresponding @ArbT@
operation. In the future, the arithmetic might be made faster by incorporating sloppy rounding (typically equivalent to a loss
of 1-2 bits of effective working precision) when the result is known to be inexact (while still propagating errors rigorously,
of course). Arithmetic operations done on exact input with exactly representable output are always guaranteed to produce exact
output.

For more complex operations, the precision parameter indicates a minimum working precision (algorithms might allocate extra
internal precision to attempt to produce an output accurate to the requested number of bits, especially when the required
precision can be estimated easily, but this is not generally required).

If the precision is increased and the inputs either are exact or are computed with increased accuracy as well, the output
should converge proportionally, absent any bugs. The general intended strategy for using ball arithmetic is to add a few guard
bits, and then repeat the calculation as necessary with an exponentially increasing number of guard bits (Ziv’s strategy) until
the result is exact enough for one’s purposes (typically the first attempt will be successful).

The following balls with an infinite or NaN component are permitted, and may be returned as output from functions.

- The ball \([+\infty \pm c]\), where \(c\) is finite, represents the point at positive infinity. Such a ball can always be replaced
  by \([+\infty \pm 0]\) while preserving mathematical correctness (this is currently not done automatically by the library).
- The ball \([-\infty \pm c]\), where \(c\) is finite, represents the point at negative infinity. Such a ball can always be replaced
  by \([-\infty \pm 0]\) while preserving mathematical correctness (this is currently not done automatically by the library).
- The ball \([c \pm \infty]\), where \(c\) is finite or infinite, represents the whole extended real line \([-\infty,+\infty]\). Such a
  ball can always be replaced by \([0 \pm \infty]\) while preserving mathematical correctness (this is currently not done automatically
  by the library). Note that there is no way to represent a half-infinite interval such as \([0,\infty]\).
- The ball \([\operatorname{NaN} \pm c]\), where \(c\) is finite or infinite, represents an indeterminate value (the value could be any
  extended real number, or it could represent a function being evaluated outside its domain of definition, for example where the result
  would be complex). Such an indeterminate ball can always be replaced by \([\operatorname{NaN} \pm \infty]\) while preserving
  mathematical correctness (this is currently not done automatically by the library).
-}

foreign import capi safe "arb.h arb_midref_" arbMidRef :: ArbT -> IO ArfT
-- ^ Macro returning a pointer to the midpoint of x as an ArfT.

foreign import capi safe "arb.h arb_radref_" arbRadRef :: ArbT -> IO MagT
-- ^ Macro returning a pointer to the radius of x as a MagT.

foreign import capi safe "arb.h value ARB_STR_MORE" arbStrMore :: CLong
-- ^ If @arbStrMore@ is added to @flags@ in a call of @arbGetStr@, more (possibly incorrect) digits may be printed.

foreign import capi safe "arb.h value ARB_STR_NO_RADIUS" arbStrNoRadius :: CLong
{- ^ If @arbStrNoRadius@ is added to @flags@ in a call of @arbGetStr@, the radius is not included in the output.
Unless @arbStrMore@ is set, the output is rounded so that the midpoint is correct to 1 ulp. As a special case,
if there are no significant digits after rounding, the result will be shown as @0e+n@, meaning that the result is between @-1e+n@ and @1e+n@
(following the contract that the output is correct to within one unit in the only shown digit).
-}

foreign import capi safe "arb.h value ARB_STR_CONDENSE" arbStrCondense :: CLong
{- ^ By adding a multiple @m@ of arbStrCondense to flags, strings of more than three times @m@ consecutive digits are
condensed, only printing the leading and trailing @m@ digits along with brackets indicating the number of digits omitted
(useful when computing values to extremely high precision).
-}

newtype Arb where
  Arb :: {_Arb :: ForeignPtr ArbC} -> Arb

instance FlintVariable Arb ArbT where
  withFlintVariable :: (ArbT -> IO b) -> Arb -> IO b
  withFlintVariable f a = withForeignPtr (_Arb a) (f . ArbT)

instance NewFlintVariable Arb ArbT where
  createNewFlintVariable :: IO Arb
  createNewFlintVariable = arbCreate

arbCreate :: IO Arb
arbCreate = do
  ArbT ptr <- arbNew
  if ptr == nullPtr
    then error "Could not allocate ArbC in arb_create"
    else do
      foreignPtr <- newForeignPtr arbDrop ptr
      return (Arb foreignPtr)

data ArbVectorC

newtype ArbVectorT where
  ArbVectorT :: {_ArbVectorT :: Ptr ArbVectorC} -> ArbVectorT
-- ^ A @Ptr ArbVectorT@ represents a vector of @ArbT@'s. In can be imaginated as a @Ptr (ArbT)@.

newtype ArbVector where
  ArbVector :: {_ArbVector :: ForeignPtr ArbVectorC} -> ArbVector

instance FlintVariable ArbVector ArbVectorT where
  withFlintVariable :: (ArbVectorT -> IO b) -> ArbVector -> IO b
  withFlintVariable f a = withForeignPtr (_ArbVector a) (f . ArbVectorT)

instance NewVectorFlintVariable ArbVector ArbVectorT where
  createNewVectorFlintVariable :: VectorLength -> IO ArbVector
  createNewVectorFlintVariable = arbCreateVector

data ArbVecEnv

foreign import capi safe "arb.h arb_vec_env_new" arb_vec_env_new :: CULong -> IO (Ptr ArbVecEnv)

arbCreateVector :: VectorLength -> IO ArbVector
arbCreateVector (VectorLength n) = do
  env <- arb_vec_env_new n
  ArbVectorT ptr <- arbVecNew n
  if ptr == nullPtr || env == nullPtr
    then error "Error in arbCreateVector allocating memory"
    else do
      foreignPtr <- newForeignPtrEnv arbVecDrop env ptr
      return (ArbVector foreignPtr)

foreign import capi safe "arb.h arb_new" arbNew :: IO ArbT
-- ^ creates a new ArbC

foreign import capi safe "arb.h &arb_drop" arbDrop :: FunPtr (Ptr ArbC -> IO ())
-- ^ destroys an ArbC

foreign import capi safe "arb.h arb_vec_new" arbVecNew :: CULong -> IO ArbVectorT
-- ^ creates a new vector of ArbC's of given length

foreign import capi safe "arb.h &arb_vec_drop" arbVecDrop :: FunPtr (Ptr ArbVecEnv -> Ptr ArbVectorC -> IO ())
-- ^ destroy a vector of  ArbC's of given length

foreign import capi safe "arb.h arb_swap"
  arbSwap ::
    -- | x
    ArbT ->
    -- | y
    ArbT ->
    IO ()
-- ^ Swaps x and y efficiently.

foreign import capi safe "arb.h arb_allocated_bytes" arbAllocatedBytes :: ArbT -> IO CLong
{- ^ Returns the total number of bytes heap-allocated internally by this object. The count excludes the size of the structure itself.
Add sizeof(arb_struct) to get the size of the object as a whole.
-}

foreign import capi safe "arb.h _arb_vec_allocated_bytes" arbVecAllocatedBytes :: ArbT -> CLong -> IO CLong
{- ^ Returns the total number of bytes allocated for this vector, i.e. the space taken up by the vector itself plus the sum of the
internal heap allocation sizes for all its member elements.
-}

foreign import capi safe "arb.h _arb_vec_estimate_allocated_bytes" arbVecEstimateAllocatedBytes :: CLong -> CLong -> IO Double
{- ^ Estimates the number of bytes that need to be allocated for a vector of len elements with prec bits of precision, including the
space for internal limb data. This function returns a double to avoid overflow issues when both len and prec are large.
-}

{- ^ This is only an approximation of the physical memory that will be used by an actual vector. In practice, the space varies
with the content of the numbers; for example, zeros and small integers require no internal heap allocation even if the precision
is huge. The estimate assumes that exponents will not be bignums. The actual amount may also be higher or lower due to overhead in
the memory allocator or overcommitment by the operating system.
-}

foreign import capi safe "arb.h arb_set"
  arbSet ::
    -- | y
    ArbT ->
    -- | x
    ArbT ->
    IO ()
-- ^ copy the value of @x@ to @y@ without rounding.

foreign import capi safe "arb.h arb_set_arf"
  arbSetArf ::
    -- | y
    ArbT ->
    -- | x
    ArfT ->
    IO ()
-- ^ copy the value of @x@ to @y@ without rounding.

foreign import capi safe "arb.h arb_set_si"
  arbSetSI ::
    -- | y
    ArbT ->
    -- | x
    CLong ->
    IO ()
-- ^ copy the value of @x@ to @y@ without rounding.

foreign import capi safe "arb.h arb_set_ui"
  arbSetUI ::
    -- | y
    ArbT ->
    -- | x
    CULong ->
    IO ()
-- ^ copy the value of @x@ to @y@ without rounding.

foreign import capi safe "arb.h arb_set_fmpz"
  arbSetFmpz ::
    -- | y
    ArbT ->
    -- | x
    FmpzT ->
    IO ()
-- ^ copy the value of @x@ to @y@ without rounding.

foreign import capi safe "arb.h arb_set_d"
  arbSetD ::
    -- | y
    ArbT ->
    -- | x
    CDouble ->
    IO ()
{- ^ copy the value of @x@ to @y@ without rounding.

Note Be cautious when using @arbSetD@ as it does not impose any error bounds and will only convert a double to an @ArbC@. For instance, @ arbSetD x 1.1@ and @arbSetStr x "1.1" prec@
work very differently, where the former will first create a double whose value is the approximation of @1.1@ (without any error bounds) which then sets @x@ to this approximated value
with no error. This differs from @arbSetStr@ which will impose an error bound based on the precision.
-}

foreign import capi safe "arb.h arb_set_fmpz_2exp"
  arbSetFmpz2Exp ::
    -- | y
    ArbT ->
    -- | x
    FmpzT ->
    -- | e
    FmpzT ->
    IO ()
-- ^ Sets @y@ to \(x2^e\).

foreign import capi safe "arb.h arb_set_round"
  arbSetRound ::
    -- | y
    ArbT ->
    -- | x
    ArbT ->
    -- | prec
    CLong ->
    IO ()
-- ^ Sets @y@ to the value of @x@, rounded to @prec@ bits in the direction towards zero.

foreign import capi safe "arb.h arb_set_round_fmpz"
  arbSetRoundFmpz ::
    -- | y
    ArbT ->
    -- | x
    FmpzT ->
    -- | prec
    CLong ->
    IO ()
-- ^ Sets @y@ to the value of @x@, rounded to @prec@ bits in the direction towards zero.

foreign import capi safe "arb.h arb_set_round_fmpz_2exp"
  arbSetRoundFmpz2Exp ::
    -- | y
    ArbT ->
    -- | x
    FmpzT ->
    -- | e
    FmpzT ->
    -- | prec
    CLong ->
    IO ()
-- ^ Sets @y@ to \(x 2^e\), rounded to @prec@ bits in the direction towards zero.

foreign import capi safe "arb.h arb_set_fmpq"
  arbSetFmpq ::
    -- | y
    ArbT ->
    -- | x
    FmpqT ->
    CLong -> -- prec
    IO ()
-- ^ Sets @y @to the rational number @x@, rounded to @prec@ bits in the direction towards zero.

foreign import capi safe "arb.h arb_set_str"
  arbSetStr ::
    -- | res
    ArbT ->
    -- | inp
    Foreign.C.String.CString ->
    -- | prec
    CLong ->
    IO CInt
{- ^ Sets @res@ to the value specified by the human-readable string @inp@. The input may be a
decimal floating-point literal, such as “25”, “0.001”, “7e+141” or “-31.4159e-1”, and
may also consist of two such literals separated by the symbol “+/-” and optionally enclosed
in brackets, e.g. “[3.25 +/- 0.0001]”, or simply “[+/- 10]” with an implicit zero midpoint.
The output is rounded to @prec@ bits, and if the binary-to-decimal conversion is inexact,
the resulting error is added to the radius.

The symbols “inf” and “nan” are recognized (a nan midpoint results in an indeterminate interval, with infinite radius).

Returns 0 if successful and nonzero if unsuccessful. If unsuccessful, the result is set to an indeterminate interval.
-}

foreign import capi safe "arb.h arb_get_str"
  arbGetStr ::
    -- | x
    ArbT ->
    -- | n
    CLong ->
    -- | flags
    CULong ->
    IO Foreign.C.String.CString
{- ^ Returns a nice human-readable representation of @x@, with at most @n@ digits of the midpoint printed.

With default flags, the output can be parsed back with @arbSetStr@, and this is guaranteed to produce
an interval containing the original interval @x@.

By default, the output is rounded so that the value given for the midpoint is correct up to 1 ulp (unit in
the last decimal place).

If @arbStrMore@ is added to @flags@ in a call of @arbGetStr@, more (possibly incorrect) digits may be printed.

If @arbStrNoRadius@ is added to @flags@ in a call of @arbGetStr@, the radius is not included in the output.
Unless @arbStrMore@ is set, the output is rounded so that the midpoint is correct to 1 ulp. As a special case,
if there are no significant digits after rounding, the result will be shown as @0e+n@, meaning that the result is between @-1e+n@ and @1e+n@
(following the contract that the output is correct to within one unit in the only shown digit).

By adding a multiple @m@ of arbStrCondense to flags, strings of more than three times @m@ consecutive digits are
condensed, only printing the leading and trailing @m@ digits along with brackets indicating the number of digits omitted
(useful when computing values to extremely high precision).
-}

foreign import capi safe "arb.h arb_zero"
  arbZero ::
    -- | x
    ArbT ->
    IO ()
-- ^ Sets @x@ to zero.

foreign import capi safe "arb.h arb_one"
  arbOne ::
    -- | x
    ArbT ->
    IO ()
-- ^ Sets @x@ to the exact integer @1@.

foreign import capi safe "arb.h arb_pos_inf"
  arbPosInf ::
    -- | x
    ArbT ->
    IO ()
-- ^ Sets @x@ to positive infinity, with a zero radius.

foreign import capi safe "arb.h arb_neg_inf"
  arbNegInf ::
    -- | x
    ArbT ->
    IO ()
-- ^ Sets @x@ to negative infinity, with a zero radius.

foreign import capi safe "arb.h arb_zero_pm_inf"
  arbZeroPMInf ::
    -- | x
    ArbT ->
    IO ()
-- ^ Sets @x@ to \([0 \pm \infty]\), representing the whole extended real line.

foreign import capi safe "arb.h arb_indeterminate"
  arbIntermediate ::
    -- | x
    ArbT ->
    IO ()
-- ^ Sets @x@ to \([\operatorname{NaN} \pm \infty]\), representing an indeterminate result.

foreign import capi safe "arb.h arb_zero_pm_one"
  arbZeroPMOne ::
    -- | x
    ArbT ->
    IO ()
-- ^ Sets @x@ to the interval \([0 \pm 1]\).

foreign import capi safe "arb.h arb_unit_interval"
  arbUnitInterval ::
    -- | x
    ArbT ->
    IO ()
-- ^ Sets @x@ to the interval \([0, 1]\).

-------------------------------------------------------------------------------

foreign import capi safe "arb.h arb_print"
  arbPrint ::
    -- | x
    ArbT ->
    IO ()
-- ^ Prints the internal representation of @x@.

foreign import capi safe "arb.h arb_printd"
  arbPrintD ::
    -- | x
    ArbT ->
    -- | d
    CLong ->
    IO ()
{- ^ Prints @x@ in decimal. The printed value of the radius is not adjusted
to compensate for the fact that the binary-to-decimal conversion
of both the midpoint and the radius introduces additional error.
-}

foreign import capi safe "arb.h arb_printn"
  arbPrintN ::
    -- | x
    ArbT ->
    -- | d
    CLong ->
    -- | f
    CLong ->
    IO ()
{- ^ Prints a nice decimal representation of @x@.
By default, the output shows the midpoint with a guaranteed error of at
most one unit in the last decimal place. In addition, an explicit error
bound is printed so that the displayed decimal interval is guaranteed to
enclose @x@.

See @arbGetStr@ for details.
-}

foreign import capi safe "arb.h arb_dump_str"
  arbDumpStr ::
    -- | x
    ArbT ->
    IO CString
{- ^ Returns a serialized representation of @x@ as a null-terminated
ASCII string that can be read by @arbLoadStr@. The format consists
of four hexadecimal integers representing the midpoint mantissa,
midpoint exponent, radius mantissa, and radius exponent (with special
values to indicate zero, infinity and NaN values),
separated by single spaces. The returned string needs to be deallocated
with @flint_free@.
-}

foreign import capi safe "arb.h arb_load_str"
  arbLoadStr ::
    -- | x
    ArbT ->
    -- | str
    CString ->
    IO CInt
{- ^ Sets @x@ to the serialized representation given in @str@. Returns a
nonzero value if @str@ is not formatted correctly (see :@arbDumpStr@).
-}

-------------------------------------------------------------------------------

foreign import capi safe "arb.h arb_randtest"
  arbRandtest ::
    -- | x
    ArbT ->
    -- | state
    FlintRandT ->
    -- | prec
    CLong ->
    -- | mag_bits
    CLong ->
    IO ()
-- ^ Generates a random ball. The midpoint and radius will both be finite.

foreign import capi safe "arb.h arb_randtest_exact"
  arbRandtestExact ::
    -- | x
    ArbT ->
    -- | state
    FlintRandT ->
    -- | prec
    CLong ->
    -- | mag_bits
    CLong ->
    IO ()
-- ^ Generates a random number with zero radius.

foreign import capi safe "arb.h arb_randtest_precise"
  arbRandtestPrecise ::
    -- | x
    ArbT ->
    -- | state
    FlintRandT ->
    -- | prec
    CLong ->
    -- | mag_bits
    CLong ->
    IO ()
-- ^ Generates a random number with radius around \(2^{\-\text{prec}}\) the magnitude of the midpoint.

foreign import capi safe "arb.h arb_randtest_positive"
  arbRandtestPositive ::
    -- | x
    ArbT ->
    -- | state
    FlintRandT ->
    -- | prec
    CLong ->
    -- | mag_bits
    CLong ->
    IO ()
-- ^ Generates a random precise number which is guaranteed to be positive.

foreign import capi safe "arb.h arb_randtest_wide"
  arbRandtestWide ::
    -- | x
    ArbT ->
    -- | state
    FlintRandT ->
    -- | prec
    CLong ->
    -- | mag_bits
    CLong ->
    IO ()
{- ^ Generates a random number with midpoint and radius chosen independently,
possibly giving a very large interval.
-}

foreign import capi safe "arb.h arb_randtest_special"
  arbRandtestSpecial ::
    -- | x
    ArbT ->
    -- | state
    FlintRandT ->
    -- | prec
    CLong ->
    -- | mag_bits
    CLong ->
    IO ()
{- ^ Generates a random interval, possibly having NaN or an infinity as the midpoint
and possibly having an infinite radius.
-}

foreign import capi safe "arb.h arb_get_rand_fmpq"
  arbGetRandFmpq ::
    -- | q
    FmpqT ->
    -- | state
    FlintRandT ->
    -- | x
    ArbT ->
    -- | bits
    CLong ->
    IO ()
{- ^ Sets @q@ to a random rational number from the interval represented by @x@.
A denominator is chosen by multiplying the binary denominator of @x@
by a random integer up to @bits@ bits.

The outcome is undefined if the midpoint or radius of @x@ is non-finite,
or if the exponent of the midpoint or radius is so large or small
that representing the endpoints as exact rational numbers would
cause overflows.
-}

foreign import capi safe "arb.h arb_urandom"
  arbUrandom ::
    -- | x
    ArbT ->
    -- | state
    FlintRandT ->
    -- | prec
    CLong ->
    IO ()
{- ^ Sets @x@ to a uniformly distributed random number in the interval
\([0, 1]\). The method uses rounding from integers to floats, hence the
radius might not be \(0\).
-}

-------------------------------------------------------------------------------

foreign import capi safe "arb.h arb_get_mid_arb"
  arbGetMidArb ::
    -- | m
    ArbT ->
    -- | x
    ArbT ->
    IO ()
-- ^ Sets @m@ to the midpoint of @x@.

foreign import capi safe "arb.h arb_get_rad_arb"
  arbGetRadArb ::
    -- | r
    ArbT ->
    -- | x
    ArbT ->
    IO ()
-- ^ Sets @r@ to the radius of @x@.

foreign import capi safe "arb.h arb_add_error_arf"
  arbAddErrorArf ::
    -- | x
    ArbT ->
    -- | err
    ArfT ->
    IO ()
-- ^ Adds the absolute value of @err@ to the radius of @x@ (the operation is done in-place).

foreign import capi safe "arb.h arb_add_error_mag"
  arbAddErrorMag ::
    -- | x
    ArbT ->
    -- | err
    MagT ->
    IO ()
-- ^ Adds the absolute value of @err@ to the radius of @x@ (the operation is done in-place).

foreign import capi safe "arb.h arb_add_error"
  arbAddError ::
    -- | x
    ArbT ->
    -- | err
    ArbT ->
    IO ()
-- ^ Adds the absolute value of @err@ to the radius of @x@ (the operation is done in-place).

foreign import capi safe "arb.h arb_add_error_2exp_si"
  arbAddError2ExpSI ::
    -- | x
    ArbT ->
    -- | e
    CLong ->
    IO ()
-- ^ Adds \(2^e\) to the radius of @x@.

foreign import capi safe "arb.h arb_add_error_2exp_fmpz"
  arbAddError2ExpFmpz ::
    -- | x
    ArbT ->
    -- | e
    FmpzT ->
    IO ()
-- ^ Adds \(2^e\) to the radius of @x@.

foreign import capi safe "arb.h arb_union"
  arbUnion ::
    -- | z
    ArbT ->
    -- | x
    ArbT ->
    -- | y
    ArbT ->
    -- | prec
    CLong ->
    IO ()
-- ^ Sets @z@ to a ball containing both @x@ and @y@.

foreign import capi safe "arb.h arb_intersection"
  arbIntersection ::
    -- | z
    ArbT ->
    -- | x
    ArbT ->
    -- | y
    ArbT ->
    -- | prec
    CLong ->
    IO CInt
{- ^ If @x@ and @y@ overlap according to @arb_overlaps@,
then @z@ is set to a ball containing the intersection of @x@ and @y@
and a nonzero value is returned.

Otherwise zero is returned and the value of @z@ is undefined.

If @x@ or @y@ contains NaN, the result is NaN.
-}

foreign import capi safe "arb.h arb_nonnegative_part"
  arbNonnegativePart ::
    -- | res
    ArbT ->
    -- | x
    ArbT ->
    IO ()
{- ^ Sets @res@ to the intersection of @x@ with \([0,\infty]\). If @x@ is
nonnegative, an exact copy is made. If @x@ is finite and contains negative
numbers, an interval of the form \([r/2 \pm r/2]\) is produced, which
certainly contains no negative points.

In the special case when @x@ is strictly negative, @res@ is set to zero.
-}

foreign import capi safe "arb.h arb_get_abs_ubound_arf"
  arbGetAbsUboundArf ::
    -- | u
    ArfT ->
    -- | x
    ArbT ->
    -- | prec
    CLong ->
    IO ()
{- ^ Sets @u@ to the upper bound for the absolute value of @x@,
rounded up to @prec@ bits. If @x@ contains NaN, the result is NaN.
-}

foreign import capi safe "arb.h arb_get_abs_lbound_arf"
  arbGetAbsLboundArf ::
    -- | u
    ArfT ->
    -- | x
    ArbT ->
    -- | prec
    CLong ->
    IO ()
{- ^ Sets @u@ to the lower bound for the absolute value of @x@,
rounded down to @prec@ bits. If @x@ contains NaN, the result is NaN.
-}

foreign import capi safe "arb.h arb_get_ubound_arf"
  arbGetUboundArf ::
    -- | u
    ArfT ->
    -- | x
    ArbT ->
    CLong -> -- prec
    IO ()
{- ^ Sets @u@ to the upper bound for the value of @x@,
rounded up to @prec@ bits. If @x@ contains NaN, the result is NaN.
-}

foreign import capi safe "arb.h arb_get_lbound_arf"
  arbGetLboundArf ::
    -- | u
    ArfT ->
    -- | x
    ArbT ->
    CLong -> -- prec
    IO ()
{- ^ Sets @u@ to the lower bound for the value of @x@,
rounded down to @prec@ bits. If @x@ contains NaN, the result is NaN.
-}

foreign import capi safe "arb.h arb_get_mag"
  arbGetMag ::
    -- | z
    MagT ->
    -- | x
    ArbT ->
    IO ()
{- ^ Sets @z@ to an upper bound for the absolute value of @x@. If @x@ contains
NaN, the result is positive infinity.
-}

foreign import capi safe "arb.h arb_get_mag_lower"
  arbGetMagLower ::
    -- | z
    MagT ->
    -- | x
    ArbT ->
    IO ()
{- ^ Sets @z@ to a lower bound for the absolute value of @x@. If @x@ contains
NaN, the result is zero.
-}

foreign import capi safe "arb.h arb_get_mag_lower_nonnegative"
  arbGetMagLowerNonnegative ::
    -- | z
    MagT ->
    -- | x
    ArbT ->
    IO ()
{- ^ Sets @z@ to a lower bound for the signed value of @x@, or zero
if @x@ overlaps with the negative half-axis. If @x@ contains NaN,
the result is zero.
-}

foreign import capi safe "arb.h arb_get_interval_fmpz_2exp"
  arbGetIntervalFmpz2Exp ::
    -- | a
    FmpzT ->
    -- | b
    FmpzT ->
    -- | exp
    FmpzT ->
    -- | x
    ArbT ->
    IO ()
{- ^ Computes the exact interval represented by @x@, in the form of an integer
interval multiplied by a power of two, i.e. \(x = [a, b] \times 2^{\text{exp}}\).
The result is normalized by removing common trailing zeros
from *a* and *b*.

This method aborts if @x@ is infinite or NaN, or if the difference between
the exponents of the midpoint and the radius is so large that allocating
memory for the result fails.

Warning: this method will allocate a huge amount of memory to store
the result if the exponent difference is huge. Memory allocation could
succeed even if the required space is far larger than the physical
memory available on the machine, resulting in swapping. It is recommended
to check that the midpoint and radius of @x@ both are within a
reasonable range before calling this method.
-}

foreign import capi safe "arb.h arb_set_interval_mag"
  arbSetIntervalMag ::
    -- | x
    ArbT ->
    -- | a
    MagT ->
    -- | b
    MagT ->
    -- | prec
    CLong ->
    IO ()
{- ^ Sets @x@ to a ball containing the interval \([a, b]\). We
require that \(a \le b\).
-}

foreign import capi safe "arb.h arb_set_interval_arf"
  arbSetIntervalArf ::
    -- | x
    ArbT ->
    -- | a
    ArfT ->
    -- | b
    ArfT ->
    -- | prec
    CLong ->
    IO ()
{- ^ Sets @x@ to a ball containing the interval \([a, b]\). We
require that \(a \le b\).
-}

foreign import capi safe "arb.h arb_set_interval_mpfr"
  arbSetIntervalMpfr ::
    -- | x
    ArbT ->
    -- | a
    MpfrT ->
    -- | b
    MpfrT ->
    -- | prec
    CLong ->
    IO ()
{- ^ Sets @x@ to a ball containing the interval \([a, b]\). We
require that \(a \le b\).
-}

foreign import capi safe "arb.h arb_set_interval_neg_pos_mag"
  arbSetIntervalNegPosMag ::
    -- | x
    ArbT ->
    -- | a
    MagT ->
    -- | b
    MagT ->
    -- | prec
    CLong ->
    IO ()
-- ^ Sets @x@ to a ball containing the interval \([-a, b]\).

foreign import capi safe "arb.h arb_get_interval_arf"
  arbGetIntervalArf ::
    -- | a
    ArfT ->
    -- | b
    ArfT ->
    -- | x
    ArbT ->
    -- | prec
    CLong ->
    IO ()
-- ^ Constructs an interval \([a, b]\) containing the ball @x@.

foreign import capi safe "arb.h arb_get_interval_mpfr"
  arbGetIntervalMpfr ::
    -- | a
    MpfrT ->
    -- | b
    MpfrT ->
    -- | x
    ArbT ->
    IO ()
{- ^ Constructs an interval \([a, b]\) containing the ball @x@. This version
uses the precision of the output variables.
-}

foreign import capi safe "arb.h arb_rel_error_bits"
  arbRefErrorBits ::
    -- | x
    ArbT ->
    IO CLong
{- ^ Returns the effective relative error of @x@ measured in bits, defined as
the difference between the position of the top bit in the radius
and the top bit in the midpoint, plus one.
The result is clamped between plus/minus @arf_prec_exact@.
-}

foreign import capi safe "arb.h arb_rel_accuracy_bits"
  arbRelEAccuracyBits ::
    -- | x
    ArbT ->
    IO CLong
{- ^ Returns the effective relative accuracy of @x@ measured in bits,
equal to the negative of the return value from @arbRefErrorBits@.
-}

foreign import capi safe "arb.h arb_rel_one_accuracy_bits"
  arbRelOneEAccuracyBits ::
    -- | x
    ArbT ->
    IO CLong
{- ^ Given a ball with midpoint @m@ and radius @r@, returns an approximation of
the relative accuracy of \([\max(1,|m|) \pm r]\) measured in bits.
-}

foreign import capi safe "arb.h arb_bits"
  arbBits ::
    -- | x
    ArbT ->
    IO CLong
{- ^ Returns the number of bits needed to represent the absolute value
of the mantissa of the midpoint of @x@, i.e, the minimum precision
sufficient to represent @x@ exactly. Returns 0 if the midpoint
of @x@ is a special value.
-}

foreign import capi safe "arb.h arb_trim"
  arbTrim ::
    -- | y
    ArbT ->
    -- | x
    ArbT ->
    IO ()
{- ^ Sets @y@ to a trimmed copy of @x@: rounds @x@ to a number of bits
equal to the accuracy of @x@ (as indicated by its radius),
plus a few guard bits. The resulting ball is guaranteed to
contain *x*, but is more economical if *x* has
less than full accuracy.
-}

foreign import capi safe "arb.h arb_get_unique_fmpz"
  arbGetUniqueFmpz ::
    -- | z
    FmpzT ->
    -- | x
    ArbT ->
    IO CInt
{- ^ If @x@ contains a unique integer, sets @z@ to that value and returns
nonzero. Otherwise (if @x@ represents no integers or more than one integer),
returns zero.

This method aborts if there is a unique integer but that integer
is so large that allocating memory for the result fails.

Warning: this method will allocate a huge amount of memory to store
the result if there is a unique integer and that integer is huge.
Memory allocation could succeed even if the required space is far
larger than the physical memory available on the machine, resulting
in swapping. It is recommended to check that the midpoint of @x@ is
within a reasonable range before calling this method.
-}

foreign import capi safe "arb.h arb_floor"
  arbFloor ::
    -- | y
    ArbT ->
    -- | x
    ArbT ->
    -- | prec
    CLong ->
    IO ()
{- ^ Sets @y@ to a ball containing \(\lfloor x \rfloor\)
with the midpoint of @y@ rounded to at most @prec@ bits.
-}

foreign import capi safe "arb.h arb_ceil"
  arbCeil ::
    -- | y
    ArbT ->
    -- | x
    ArbT ->
    -- | prec
    CLong ->
    IO ()
{- ^ Sets @y@ to a ball containing
\(\lceil x \rceil\) with the midpoint of @y@ rounded to at most @prec@ bits.
-}

foreign import capi safe "arb.h arb_trunc"
  arbTrunc ::
    -- | y
    ArbT ->
    -- | x
    ArbT ->
    -- | prec
    CLong ->
    IO ()
{- ^ Sets @y@ to a ball containing \(\operatorname{trunc}(x)\)
with the midpoint of @y@ rounded to at most @prec@ bits.
-}

foreign import capi safe "arb.h arb_nint"
  arbNint ::
    -- | y
    ArbT ->
    -- | x
    ArbT ->
    -- | prec
    CLong ->
    IO ()
{- ^ Sets @y@ to a ball containing \(\operatorname{nint}(x)\),
with the midpoint of @y@ rounded to at most @prec@ bits.
-}

foreign import capi safe "arb.h arb_get_fmpz_mid_rad_10exp"
  arbGetFmpzMidRad10Exp ::
    -- | mid
    FmpzT ->
    -- | rad
    FmpzT ->
    -- | exp
    FmpzT ->
    -- | x
    ArbT ->
    -- | n
    CLong ->
    IO ()
{- ^ Assuming that @x@ is finite and not exactly zero, computes integers @mid@,
@rad@, and @exp@ such that \(x \in [m-rad, m+r] \times 10^e\) and such that the
larger out of @mid@ and @rad@ has at least @n@ digits plus a few guard
digits. If @x@ is infinite or exactly zero, the outputs are all set
to zero.
-}

foreign import capi safe "arb.h arb_can_round_arf"
  arbCanRoundArf ::
    -- | x
    ArbT ->
    -- | prec
    CLong ->
    -- | rnd
    ArfRndC ->
    IO CInt
{- ^ Returns nonzero if rounding the midpoint of @x@ to @prec@ bits in
the direction @rnd@ is guaranteed to give the unique correctly
rounded floating-point approximation for the real number represented by @x@.

In other words, if this function returns nonzero, applying
@arf_set_round@, or @arf_get_mpfr@, or @arf_get_d@
to the midpoint of @x@ is guaranteed to return a correctly rounded @ArfT@,
@mpfr_t@ (provided that @prec@ is the precision of the output variable),
or @double@ (provided that *prec* is 53).

Moreover, @arf_get_mpfr@ is guaranteed to return the correct ternary
value according to MPFR semantics.

Note that the @mpfr@ version of this function takes an MPFR rounding mode
symbol as input, while the @arf@ version takes an @arf@ rounding mode
symbol. Otherwise, the functions are identical.

This function may perform a fast, inexact test; that is, it may return
zero in some cases even when correct rounding actually is possible.

To be conservative, zero is returned when @x@ is non-finite, even if it
is an "exact" infinity.
-}

foreign import capi safe "arb.h arb_can_round_mpfr"
  arbCanRoundMpfr ::
    -- | x
    ArbT ->
    -- | prec
    CLong ->
    -- | rnd
    MpfrRndC ->
    IO CInt
{- ^ Returns nonzero if rounding the midpoint of @x@ to @prec@ bits in
the direction @rnd@ is guaranteed to give the unique correctly
rounded floating-point approximation for the real number represented by @x@.

In other words, if this function returns nonzero, applying
@arf_set_round@, or @arf_get_mpfr@, or @arf_get_d@
to the midpoint of @x@ is guaranteed to return a correctly rounded @ArfT@,
@mpfr_t@ (provided that @prec@ is the precision of the output variable),
or @double@ (provided that *prec* is 53).

Moreover, @arf_get_mpfr@ is guaranteed to return the correct ternary
value according to MPFR semantics.

Note that the @mpfr@ version of this function takes an MPFR rounding mode
symbol as input, while the @arf@ version takes an @arf@ rounding mode
symbol. Otherwise, the functions are identical.

This function may perform a fast, inexact test; that is, it may return
zero in some cases even when correct rounding actually is possible.

To be conservative, zero is returned when @x@ is non-finite, even if it
is an "exact" infinity.
-}
