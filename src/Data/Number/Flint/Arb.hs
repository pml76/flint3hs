{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
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

  -- * Comparisons
  arbIsZero,
  arbIsNonzero,
  arbIsOne,
  arbIsFinite,
  arbIsExact,
  arbIsInt,
  arbIsInt2ExpSI,
  arbEqual,
  arbEqualSI,
  arbIsPositive,
  arbIsNonnegative,
  arbIsNegative,
  arbIsNonpositive,
  arbOverlaps,
  arbContains,
  arbContainsArf,
  arbContainsFmpq,
  arbContainsFmpz,
  arbContainsSI,
  arbContainsMpfr,
  arbContainsInt,
  arbContainsZero,
  arbContainsNegative,
  arbContainsNonpositive,
  arbContainsPositive,
  arbContainsNonnegative,
  arbContainsInterior,
  arbEq,
  arbNe,
  arbLt,
  arbGe,
  arbLe,
  arbGt,

  -- * Arithmetic
  arbNeg,
  arbNegRound,
  arbAbs,
  arbNonnegativeAbs,
  arbSgn,
  arbSgnNonzero,
  arbMin,
  arbMax,
  arbMinMax,
  arbAdd,
  arbAddArf,
  arbAddUI,
  arbAddSI,
  arbAddFmpz,
  arbAddFmpz2Exp,
  arbSub,
  arbSubArf,
  arbSubSI,
  arbSubUI,
  arbSubFmpz,
  arbMul,
  arbMulArf,
  arbMulSI,
  arbMulUI,
  arbMulFmpz,
  arbMul2ExpFmpz,
  arbMul2ExpSI,
  arbAddMul,
  arbAddMulArf,
  arbAddMulSI,
  arbAddMulUI,
  arbAddMulFmpz,
  arbSubMul,
  arbSubMulArf,
  arbSubMulSI,
  arbSubMulUI,
  arbSubMulFmpz,
  arbFma,
  arbFmaArf,
  arbFmaSI,
  arbFmaUI,
  arbFmaFmpz,
  arbInv,
  arbDiv,
  arbDivArf,
  arbDivSI,
  arbDivUI,
  arbDivFmpz,
  arbFmpzDivFmpz,
  arbUIDiv,
  arbDiv2ExpM1UI,

  -- * Dot product
  arbDot,
  arbDotPrecise,
  arbDotSimple,
  arbApproxDot,
  arbDotUI,
  arbDotUIUI,
  arbDotSI,
  arbDotSIUI,
  arbDotFmpz,

  -- * Powers and roots
  arbSqrt,
  arbSqrt1PM1,
  arbSqrtArf,
  arbSqrtFmpz,
  arbSqrtUI,
  arbSqrtpos,
  arbHypot,
  arbRsqrt,
  arbRsqrtUI,
  arbRoot,
  arbRootUI,
  arbSqr,
  arbPow,
  arbPowFmpzBinexp,
  arbPowFmpz,
  arbPowUI,
  arbUIPowUI,
  arbSIPowUI,
  arbPowFmpq,
) where

import Data.Number.Flint.Arf (ArfRndC (..), ArfT (..))
import Data.Number.Flint.Flint (FlintRandT (..))
import Data.Number.Flint.FlintVariable
import Data.Number.Flint.Fmpq (FmpqT (..))
import Data.Number.Flint.Fmpz (FmpzT (..), FmpzVectorT(..))
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
import Data.Number.Flint.TH
import Foreign.C ( CULLong(..), CLLong(..) )


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

$(generateFFIBindings cFunctions)
-- foreign import capi safe "arb.h arb_new" arbNew :: IO ArbT
-- ^ creates a new ArbC


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
contain @x@, but is more economical if @x@ has
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
or @double@ (provided that @prec@ is 53).

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
or @double@ (provided that @prec@ is 53).

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

-------------------------------------------------------------------------------

foreign import capi safe "arb.h arb_is_zero"
  arbIsZero ::
    -- | x
    ArbT ->
    IO CInt
-- ^ Returns nonzero iff the midpoint and radius of @x@ are both zero.

foreign import capi safe "arb.h arb_is_nonzero"
  arbIsNonzero ::
    -- | x
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff zero is not contained in the interval represented
by @x@.
-}

foreign import capi safe "arb.h arb_is_one"
  arbIsOne ::
    -- | x
    ArbT ->
    IO CInt
-- ^ Returns nonzero iff @x@ is exactly 1.

foreign import capi safe "arb.h arb_is_finite"
  arbIsFinite ::
    -- | x
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff the midpoint and radius of @x@ are both finite
floating-point numbers, i.e. not infinities or NaN.
-}

foreign import capi safe "arb.h arb_is_exact"
  arbIsExact ::
    -- | x
    ArbT ->
    IO CInt
-- ^ Returns nonzero iff the radius of @x@ is zero.

foreign import capi safe "arb.h arb_is_int"
  arbIsInt ::
    -- | x
    ArbT ->
    IO CInt
-- ^ Returns nonzero iff @x@ is an exact integer.

foreign import capi safe "arb.h arb_is_int_2exp_si"
  arbIsInt2ExpSI ::
    -- | x
    ArbT ->
    -- | e
    CLong ->
    IO CInt
-- ^ Returns nonzero iff @x@ exactly equals \(n 2^e\) for some integer @n@.

foreign import capi safe "arb.h arb_equal"
  arbEqual ::
    -- | x
    ArbT ->
    -- | y
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff @x@ and @y@ are equal as balls, i.e. have both the
    same midpoint and radius.

    Note that this is not the same thing as testing whether both
    @x@ and @y@ certainly represent the same real number, unless
    either @x@ or @y@ is exact (and neither contains NaN).
    To test whether both operands *might* represent the same mathematical
    quantity, use @arb_overlaps@ or @arb_contains@,
    depending on the circumstance.
-}

foreign import capi safe "arb.h arb_equal_si"
  arbEqualSI ::
    -- | x
    ArbT ->
    -- | y
    CLong ->
    IO CInt
-- ^ Returns nonzero iff @x@ is equal to the integer @y@.

foreign import capi safe "arb.h arb_is_positive"
  arbIsPositive ::
    -- | x
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff all points @p@ in the interval represented by @x@
    satisfy \(p > 0\).
    If @x@ contains NaN, returns zero.
-}

foreign import capi safe "arb.h arb_is_nonnegative"
  arbIsNonnegative ::
    -- | x
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff all points @p@ in the interval represented by @x@
    satisfy \(p \ge 0\).
    If @x@ contains NaN, returns zero.
-}

foreign import capi safe "arb.h arb_is_negative"
  arbIsNegative ::
    -- | x
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff all points @p@ in the interval represented by @x@
    satisfy \(p < 0\).
    If @x@ contains NaN, returns zero.
-}

foreign import capi safe "arb.h arb_is_nonpositive"
  arbIsNonpositive ::
    -- | x
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff all points @p@ in the interval represented by @x@
    satisfy \(p \le 0\).
    If @x@ contains NaN, returns zero.
-}

foreign import capi safe "arb.h arb_overlaps"
  arbOverlaps ::
    -- | x
    ArbT ->
    -- | y
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff @x@ and @y@ have some point in common.
    If either @x@ or @y@ contains NaN, this function always returns nonzero
    (as a NaN could be anything, it could in particular contain any
    number that is included in the other operand).
-}

foreign import capi safe "arb.h arb_contains_arf"
  arbContainsArf ::
    -- | x
    ArbT ->
    -- | y
    ArfT ->
    IO CInt
{- ^ Returns nonzero iff the given number (or ball) @y@ is contained in
    the interval represented by @x@.

    If @x@ contains NaN, this function always returns nonzero (as it
    could represent anything, and in particular could represent all
    the points included in @y@).
    If @y@ contains NaN and @x@ does not, it always returns zero.
-}

foreign import capi safe "arb.h arb_contains_fmpq"
  arbContainsFmpq ::
    -- | x
    ArbT ->
    -- | y
    FmpqT ->
    IO CInt
{- ^ Returns nonzero iff the given number (or ball) @y@ is contained in
    the interval represented by @x@.

    If @x@ contains NaN, this function always returns nonzero (as it
    could represent anything, and in particular could represent all
    the points included in @y@).
    If @y@ contains NaN and @x@ does not, it always returns zero.
-}

foreign import capi safe "arb.h arb_contains_fmpz"
  arbContainsFmpz ::
    -- | x
    ArbT ->
    -- | y
    FmpzT ->
    IO CInt
{- ^ Returns nonzero iff the given number (or ball) @y@ is contained in
    the interval represented by @x@.

    If @x@ contains NaN, this function always returns nonzero (as it
    could represent anything, and in particular could represent all
    the points included in @y@).
    If @y@ contains NaN and @x@ does not, it always returns zero.
-}

foreign import capi safe "arb.h arb_contains_si"
  arbContainsSI ::
    -- | x
    ArbT ->
    -- | y
    CLong ->
    IO CInt
{- ^ Returns nonzero iff the given number (or ball) @y@ is contained in
    the interval represented by @x@.

    If @x@ contains NaN, this function always returns nonzero (as it
    could represent anything, and in particular could represent all
    the points included in @y@).
    If @y@ contains NaN and @x@ does not, it always returns zero.
-}

foreign import capi safe "arb.h arb_contains_mpfr"
  arbContainsMpfr ::
    -- | x
    ArbT ->
    -- | y
    MpfrT ->
    IO CInt
{- ^ Returns nonzero iff the given number (or ball) @y@ is contained in
    the interval represented by @x@.

    If @x@ contains NaN, this function always returns nonzero (as it
    could represent anything, and in particular could represent all
    the points included in @y@).
    If @y@ contains NaN and @x@ does not, it always returns zero.
-}

foreign import capi safe "arb.h arb_contains"
  arbContains ::
    -- | x
    ArbT ->
    -- | y
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff the given number (or ball) @y@ is contained in
    the interval represented by @x@.

    If @x@ contains NaN, this function always returns nonzero (as it
    could represent anything, and in particular could represent all
    the points included in @y@).
    If @y@ contains NaN and @x@ does not, it always returns zero.
-}

foreign import capi safe "arb.h arb_contains_int"
  arbContainsInt ::
    -- | x
    ArbT ->
    IO CInt
-- ^ Returns nonzero iff the interval represented by @x@ contains an integer.

foreign import capi safe "arb.h arb_contains_zero"
  arbContainsZero ::
    -- | x
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff there is any point @p@ in the interval represented
    by @x@ satisfying \(p = 0\).
    If @x@ contains NaN, returns nonzero.
-}

foreign import capi safe "arb.h arb_contains_negative"
  arbContainsNegative ::
    -- | x
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff there is any point @p@ in the interval represented
    by @x@ satisfying \(p < 0\).
    If @x@ contains NaN, returns nonzero.
-}

foreign import capi safe "arb.h arb_contains_nonpositive"
  arbContainsNonpositive ::
    -- | x
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff there is any point @p@ in the interval represented
    by @x@ satisfying \(p \le 0\).
    If @x@ contains NaN, returns nonzero.
-}

foreign import capi safe "arb.h arb_contains_positive"
  arbContainsPositive ::
    -- | x
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff there is any point @p@ in the interval represented
    by @x@ satisfying \(p > 0\).
    If @x@ contains NaN, returns nonzero.
-}

foreign import capi safe "arb.h arb_contains_nonnegative"
  arbContainsNonnegative ::
    -- | x
    ArbT ->
    IO CInt
{- ^ Returns nonzero iff there is any point @p@ in the interval represented
    by @x@ satisfying \(p \ge 0\).
    If @x@ contains NaN, returns nonzero.
-}

foreign import capi safe "arb.h arb_contains_interior"
  arbContainsInterior ::
    -- |  x
    ArbT ->
    -- | y
    ArbT ->
    IO CInt
-- ^ Tests if @y@ is contained in the interior of @x@; that is, contained in @x@ and not touching either endpoint.

foreign import capi safe "arb.h arb_eq"
  arbEq ::
    -- |  x
    ArbT ->
    -- | y
    ArbT ->
    IO CInt
{- ^ Performs the comparison \(x = y\) in a mathematically meaningful way.
    If the comparison \(t \, (\operatorname{op}) \, u\) holds for all
    \(t \in x\) and all \(u \in y\), returns 1.
    Otherwise, returns 0.

    The balls @x@ and @y@ are viewed as subintervals of the extended real line.
    Note that balls that are formally different can compare as equal
    under this definition: for example, \([-\infty \pm 3] = [-\infty \pm 0]\).
    Also \([-\infty] \le [\infty \pm \infty]\).

    The output is always 0 if either input has NaN as midpoint.
-}

foreign import capi safe "arb.h arb_ne"
  arbNe ::
    -- |  x
    ArbT ->
    -- | y
    ArbT ->
    IO CInt
{- ^ Performs the comparison \(x \ne y\) in a mathematically meaningful way.
    If the comparison \(t \, (\operatorname{op}) \, u\) holds for all
    \(t \in x\) and all \(u \in y\), returns 1.
    Otherwise, returns 0.

    The balls @x@ and @y@ are viewed as subintervals of the extended real line.
    Note that balls that are formally different can compare as equal
    under this definition: for example, \([-\infty \pm 3] = [-\infty \pm 0]\).
    Also \([-\infty] \le [\infty \pm \infty]\).

    The output is always 0 if either input has NaN as midpoint.
-}

foreign import capi safe "arb.h arb_lt"
  arbLt ::
    -- |  x
    ArbT ->
    -- | y
    ArbT ->
    IO CInt
    
{- ^ Performs the comparison \(x < y\) in a mathematically meaningful way.
    If the comparison \(t \, (\operatorname{op}) \, u\) holds for all
    \(t \in x\) and all \(u \in y\), returns 1.
    Otherwise, returns 0.

    The balls @x@ and @y@ are viewed as subintervals of the extended real line.
    Note that balls that are formally different can compare as equal
    under this definition: for example, \([-\infty \pm 3] = [-\infty \pm 0]\).
    Also \([-\infty] \le [\infty \pm \infty]\).

    The output is always 0 if either input has NaN as midpoint.
-}

foreign import capi safe "arb.h arb_le"
  arbLe ::
    -- |  x
    ArbT ->
    -- | y
    ArbT ->
    IO CInt
{- ^ Performs the comparison \(x \le y\) in a mathematically meaningful way.
    If the comparison \(t \, (\operatorname{op}) \, u\) holds for all
    \(t \in x\) and all \(u \in y\), returns 1.
    Otherwise, returns 0.

    The balls @x@ and @y@ are viewed as subintervals of the extended real line.
    Note that balls that are formally different can compare as equal
    under this definition: for example, \([-\infty \pm 3] = [-\infty \pm 0]\).
    Also \([-\infty] \le [\infty \pm \infty]\).

    The output is always 0 if either input has NaN as midpoint.
-}

foreign import capi safe "arb.h arb_gt"
  arbGt ::
    -- |  x
    ArbT ->
    -- | y
    ArbT ->
    IO CInt
{- ^ Performs the comparison \(x > y\) in a mathematically meaningful way.
    If the comparison \(t \, (\operatorname{op}) \, u\) holds for all
    \(t \in x\) and all \(u \in y\), returns 1.
    Otherwise, returns 0.

    The balls @x@ and @y@ are viewed as subintervals of the extended real line.
    Note that balls that are formally different can compare as equal
    under this definition: for example, \([-\infty \pm 3] = [-\infty \pm 0]\).
    Also \([-\infty] \le [\infty \pm \infty]\).

    The output is always 0 if either input has NaN as midpoint.
-}

foreign import capi safe "arb.h arb_ge"
  arbGe ::
    -- |  x
    ArbT ->
    -- | y
    ArbT ->
    IO CInt
{- ^ Performs the comparison \(x \ge y\) in a mathematically meaningful way.
    If the comparison \(t \, (\operatorname{op}) \, u\) holds for all
    \(t \in x\) and all \(u \in y\), returns 1.
    Otherwise, returns 0.

    The balls @x@ and @y@ are viewed as subintervals of the extended real line.
    Note that balls that are formally different can compare as equal
    under this definition: for example, \([-\infty \pm 3] = [-\infty \pm 0]\).
    Also \([-\infty] \le [\infty \pm \infty]\).

    The output is always 0 if either input has NaN as midpoint.
-}




-------------------------------------------------------------------------------

foreign import capi safe "arb.h arb_neg" arbNeg :: ArbT -- ^ y
  -> ArbT -- ^ x 
  -> IO ()
-- ^ Sets @y@ to the negation of @x@.

foreign import capi safe "arb.h arb_neg_round" arbNegRound :: ArbT -- ^ y
  -> ArbT -- ^ x 
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @y@ to the negation of @x@.

foreign import capi safe "arb.h arb_abs" arbAbs :: ArbT -- ^ y
  -> ArbT -- ^ x 
  -> IO ()
-- ^ Sets @y@ to the absolute value of @x@. No attempt is made to improve the
-- interval represented by @x@ if it contains zero.

foreign import capi safe "arb.h arb_nonnegative_abs" arbNonnegativeAbs :: ArbT -- ^ y
  -> ArbT -- ^ x 
  -> IO ()
-- ^ Sets @y@ to the absolute value of @x@. If @x@ is finite and it contains
-- zero, sets @y@ to some interval `[r \pm r]` that contains the absolute
-- value of @x@.

foreign import capi safe "arb.h arb_sgn" arbSgn :: ArbT -- ^ y
  -> ArbT -- ^ x 
  -> IO ()
-- ^ Sets @y@ to the sign function of @x@. The result is \([0 \pm 1]\) if
-- @x@ contains both zero and nonzero numbers.

foreign import capi safe "arb.h arb_sgn_nonzero" arbSgnNonzero :: ArbT -- ^ x 
  -> IO CInt
-- ^ Returns 1 if @x@ is strictly positive, -1 if @x@ is strictly negative,
-- ^ and 0 if @x@ is zero or a ball containing zero so that its sign
-- is not determined.

foreign import capi safe "arb.h arb_min" arbMin :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @z@ to the maximum of @x@ and @y@.

foreign import capi safe "arb.h arb_max" arbMax :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @z@ to the maximum of @x@ and @y@.


foreign import capi safe "arb.h arb_minmax" arbMinMax :: ArbT -- ^ z1
  -> ArbT -- ^ z2
  -> ArbT -- ^ x 
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @z1@ and @z2@ respectively to the minimum and the maximum of @x@ and @y@.

foreign import capi safe "arb.h arb_add" arbAdd :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x + y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_add_arf" arbAddArf :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArfT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x + y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_add_ui" arbAddUI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CULong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x + y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_add_si" arbAddSI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CLong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x + y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_add_fmpz" arbAddFmpz :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> FmpzT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x + y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.



foreign import capi safe "arb.h arb_add_fmpz_2exp" arbAddFmpz2Exp :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> FmpzT -- ^ m
  -> FmpzT -- ^ e
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets `z = x + m \cdot 2^e`, rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.



foreign import capi safe "arb.h arb_sub" arbSub :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x - y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_sub_arf" arbSubArf :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArfT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x - y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_sub_ui" arbSubUI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CULong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x - y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_sub_si" arbSubSI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CLong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x - y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_sub_fmpz" arbSubFmpz :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> FmpzT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x - y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_mul" arbMul :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_mul_arf" arbMulArf :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArfT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_mul_si" arbMulSI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CLong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_mul_ui" arbMulUI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CULong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_mul_fmpz" arbMulFmpz :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> FmpzT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_mul_2exp_si" arbMul2ExpSI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CLong -- ^ e
  -> IO ()
-- ^ Sets @y@ to @x@ multiplied by \(2^e\).

foreign import capi safe "arb.h arb_mul_2exp_fmpz" arbMul2ExpFmpz :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> FmpzT -- ^ e
  -> IO ()
-- ^ Sets @y@ to @x@ multiplied by \(2^e\).

foreign import capi safe "arb.h arb_addmul" arbAddMul :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = z + x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_addmul_arf" arbAddMulArf :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArfT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = z + x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_addmul_si" arbAddMulSI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CLong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = z + x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_addmul_ui" arbAddMulUI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CULong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = z + x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_addmul_fmpz" arbAddMulFmpz :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> FmpzT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = z + x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_submul" arbSubMul :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = z - x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_submul_arf" arbSubMulArf :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArfT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = z - x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_submul_si" arbSubMulSI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CLong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = z - x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_submul_ui" arbSubMulUI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CULong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = z - x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_submul_fmpz" arbSubMulFmpz :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> FmpzT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = z - x \cdot y\), rounded to @prec@ bits. The precision can be
-- @arf_prec_exact@ provided that the result fits in memory.

foreign import capi safe "arb.h arb_fma" arbFma :: ArbT -- ^ res
  -> ArbT -- ^ x
  -> ArbT -- ^ y
  -> ArbT -- ^ z
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @res@ to \(x \cdot y + z`\). This is equivalent to an @addmul@ except
-- that @res@ and @z@ can be separate variables.

foreign import capi safe "arb.h arb_fma_arf" arbFmaArf :: ArbT -- ^ res
  -> ArbT -- ^ x
  -> ArfT -- ^ y
  -> ArbT -- ^ z
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @res@ to \(x \cdot y + z`\). This is equivalent to an @addmul@ except
-- that @res@ and @z@ can be separate variables.

foreign import capi safe "arb.h arb_fma_si" arbFmaSI :: ArbT -- ^ res
  -> ArbT -- ^ x
  -> CLong -- ^ y
  -> ArbT -- ^ z
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @res@ to \(x \cdot y + z`\). This is equivalent to an @addmul@ except
-- that @res@ and @z@ can be separate variables.

foreign import capi safe "arb.h arb_fma_ui" arbFmaUI :: ArbT -- ^ res
  -> ArbT -- ^ x
  -> CULong -- ^ y
  -> ArbT -- ^ z
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @res@ to \(x \cdot y + z`\). This is equivalent to an @addmul@ except
-- that @res@ and @z@ can be separate variables.

foreign import capi safe "arb.h arb_fma_fmpz" arbFmaFmpz :: ArbT -- ^ res
  -> ArbT -- ^ x
  -> FmpzT -- ^ y
  -> ArbT -- ^ z
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @res@ to \(x \cdot y + z`\). This is equivalent to an @addmul@ except
-- that @res@ and @z@ can be separate variables.


foreign import capi safe "arb.h arb_inv" arbInv :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @z@ to \(1 / x\).

foreign import capi safe "arb.h arb_div" arbDiv :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(z = x / y\), rounded to @prec@ bits. If @y@ contains zero, @z@ is
    set to \(0 \pm \infty\). Otherwise, error propagation uses the rule

    \[
        \left| \frac{x}{y} - \frac{x+\xi_1 a}{y+\xi_2 b} \right| =
        \left|\frac{x \xi_2 b - y \xi_1 a}{y (y+\xi_2 b)}\right| \le
        \frac{|xb|+|ya|}{|y| (|y|-b)}
    \]

    where \(-1 \le \xi_1, \xi_2 \le 1\), and
    where the triangle inequality has been applied to the numerator and
    the reverse triangle inequality has been applied to the denominator. -}

foreign import capi safe "arb.h arb_div_arf" arbDivArf :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> ArfT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(z = x / y\), rounded to @prec@ bits. If @y@ contains zero, @z@ is
    set to \(0 \pm \infty\). Otherwise, error propagation uses the rule

    \[
        \left| \frac{x}{y} - \frac{x+\xi_1 a}{y+\xi_2 b} \right| =
        \left|\frac{x \xi_2 b - y \xi_1 a}{y (y+\xi_2 b)}\right| \le
        \frac{|xb|+|ya|}{|y| (|y|-b)}
    \]

    where \(-1 \le \xi_1, \xi_2 \le 1\), and
    where the triangle inequality has been applied to the numerator and
    the reverse triangle inequality has been applied to the denominator. -}

foreign import capi safe "arb.h arb_div_si" arbDivSI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CLong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(z = x / y\), rounded to @prec@ bits. If @y@ contains zero, @z@ is
    set to \(0 \pm \infty\). Otherwise, error propagation uses the rule

    \[
        \left| \frac{x}{y} - \frac{x+\xi_1 a}{y+\xi_2 b} \right| =
        \left|\frac{x \xi_2 b - y \xi_1 a}{y (y+\xi_2 b)}\right| \le
        \frac{|xb|+|ya|}{|y| (|y|-b)}
    \]

    where \(-1 \le \xi_1, \xi_2 \le 1\), and
    where the triangle inequality has been applied to the numerator and
    the reverse triangle inequality has been applied to the denominator. -}

foreign import capi safe "arb.h arb_div_ui" arbDivUI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CULong -- ^ y
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(z = x / y\), rounded to @prec@ bits. If @y@ contains zero, @z@ is
    set to \(0 \pm \infty\). Otherwise, error propagation uses the rule

    \[
        \left| \frac{x}{y} - \frac{x+\xi_1 a}{y+\xi_2 b} \right| =
        \left|\frac{x \xi_2 b - y \xi_1 a}{y (y+\xi_2 b)}\right| \le
        \frac{|xb|+|ya|}{|y| (|y|-b)}
    \]

    where \(-1 \le \xi_1, \xi_2 \le 1\), and
    where the triangle inequality has been applied to the numerator and
    the reverse triangle inequality has been applied to the denominator. -}

foreign import capi safe "arb.h arb_div_fmpz" arbDivFmpz :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> FmpzT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(z = x / y\), rounded to @prec@ bits. If @y@ contains zero, @z@ is
    set to \(0 \pm \infty\). Otherwise, error propagation uses the rule

    \[
        \left| \frac{x}{y} - \frac{x+\xi_1 a}{y+\xi_2 b} \right| =
        \left|\frac{x \xi_2 b - y \xi_1 a}{y (y+\xi_2 b)}\right| \le
        \frac{|xb|+|ya|}{|y| (|y|-b)}
    \]

    where \(-1 \le \xi_1, \xi_2 \le 1\), and
    where the triangle inequality has been applied to the numerator and
    the reverse triangle inequality has been applied to the denominator. -}

foreign import capi safe "arb.h arb_fmpz_div_fmpz" arbFmpzDivFmpz :: ArbT -- ^ z
  -> FmpzT -- ^ x 
  -> FmpzT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(z = x / y\), rounded to @prec@ bits. If @y@ contains zero, @z@ is
    set to \(0 \pm \infty\). Otherwise, error propagation uses the rule

    \[
        \left| \frac{x}{y} - \frac{x+\xi_1 a}{y+\xi_2 b} \right| =
        \left|\frac{x \xi_2 b - y \xi_1 a}{y (y+\xi_2 b)}\right| \le
        \frac{|xb|+|ya|}{|y| (|y|-b)}
    \]

    where \(-1 \le \xi_1, \xi_2 \le 1\), and
    where the triangle inequality has been applied to the numerator and
    the reverse triangle inequality has been applied to the denominator. -}

foreign import capi safe "arb.h arb_ui_div" arbUIDiv :: ArbT -- ^ z
  -> CLong -- ^ x 
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(z = x / y\), rounded to @prec@ bits. If @y@ contains zero, @z@ is
    set to \(0 \pm \infty\). Otherwise, error propagation uses the rule

    \[
        \left| \frac{x}{y} - \frac{x+\xi_1 a}{y+\xi_2 b} \right| =
        \left|\frac{x \xi_2 b - y \xi_1 a}{y (y+\xi_2 b)}\right| \le
        \frac{|xb|+|ya|}{|y| (|y|-b)}
    \]

    where \(-1 \le \xi_1, \xi_2 \le 1\), and
    where the triangle inequality has been applied to the numerator and
    the reverse triangle inequality has been applied to the denominator. -}

foreign import capi safe "arb.h arb_div_2expm1_ui" arbDiv2ExpM1UI :: ArbT -- ^ z
  -> ArbT -- ^ x 
  -> CULong -- ^ n
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = x / (2^n - 1)\), rounded to @prec@ bits.


-------------------------------------------------------------------------------

foreign import capi safe "arb.h arb_dot_precise" arbDotPrecise :: ArbT -- ^ res 
  -> ArbT -- ^ s 
  -> CInt -- ^ subtract
  -> ArbVectorT -- ^ x 
  -> CLong -- ^ xstep 
  -> ArbVectorT -- ^ y
  -> CLong -- ^ ystep
  -> CLong -- ^ len
  -> CLong -- ^ prec 
  -> IO ()
{- ^ Computes the dot product of the vectors @x@ and @y@, setting
    @res@ to \(s + (-1)^{subtract} \sum_{i=0}^{len-1} x_i y_i\).

    The parameter @subtract@ must be 0 or 1.
    The length @len@ is allowed to be negative, which is equivalent
    to a length of zero.
    The parameters @xstep@ or @ystep@ specify a step length for
    traversing subsequences of the vectors @x@ and @y@; either can be
    negative to step in the reverse direction starting from
    the initial pointer.
    Aliasing is allowed between @res@ and @s@ but not between
    @res@ and the entries of @x@ and @y@.

    This version computes the dot product exactly up to the
    final rounding. This can be extremely slow and is only intended
    for testing. -}


foreign import capi safe "arb.h arb_dot_simple" arbDotSimple :: ArbT -- ^ res 
  -> ArbT -- ^ s 
  -> CInt -- ^ subtract
  -> ArbVectorT -- ^ x 
  -> CLong -- ^ xstep 
  -> ArbVectorT -- ^ y
  -> CLong -- ^ ystep
  -> CLong -- ^ len
  -> CLong -- ^ prec 
  -> IO ()
{- ^ Computes the dot product of the vectors @x@ and @y@, setting
    @res@ to \(s + (-1)^{subtract} \sum_{i=0}^{len-1} x_i y_i\).

    The parameter @subtract@ must be 0 or 1.
    The length @len@ is allowed to be negative, which is equivalent
    to a length of zero.
    The parameters @xstep@ or @ystep@ specify a step length for
    traversing subsequences of the vectors @x@ and @y@; either can be
    negative to step in the reverse direction starting from
    the initial pointer.
    Aliasing is allowed between @res@ and @s@ but not between
    @res@ and the entries of @x@ and @y@.

    This version performs fused multiply-add operations in
    a simple loop. This can be used for
    testing purposes and is also used as a fallback by the
    default version when the exponents are out of range
    for the optimized code. -}


foreign import capi safe "arb.h arb_dot" arbDot :: ArbT -- ^ res 
  -> ArbT -- ^ s 
  -> CInt -- ^ subtract
  -> ArbVectorT -- ^ x 
  -> CLong -- ^ xstep 
  -> ArbVectorT -- ^ y
  -> CLong -- ^ ystep
  -> CLong -- ^ len
  -> CLong -- ^ prec 
  -> IO ()

{- ^ Computes the dot product of the vectors @x@ and @y@, setting
    @res@ to \(s + (-1)^{subtract} \sum_{i=0}^{len-1} x_i y_i\).

    The parameter @subtract@ must be 0 or 1.
    The length @len@ is allowed to be negative, which is equivalent
    to a length of zero.
    The parameters @xstep@ or @ystep@ specify a step length for
    traversing subsequences of the vectors @x@ and @y@; either can be
    negative to step in the reverse direction starting from
    the initial pointer.
    Aliasing is allowed between @res@ and @s@ but not between
    @res@ and the entries of @x@ and @y@.

    The version (a.k.a the default version) determines the optimal precision for each term
    and performs all internal calculations using mpn arithmetic
    with minimal overhead. This is the preferred way to compute a
    dot product; it is generally much faster and more precise
    than a simple loop. -}

foreign import capi safe "arb.h arb_approx_dot" arbApproxDot :: ArbT -- ^ res 
  -> ArbT -- ^ s 
  -> CInt -- ^ subtract
  -> ArbVectorT -- ^ x 
  -> CLong -- ^ xstep 
  -> ArbVectorT -- ^ y
  -> CLong -- ^ ystep
  -> CLong -- ^ len
  -> CLong -- ^ prec 
  -> IO ()
{- ^ Computes an approximate dot product *without error bounds*.
    The radii of the inputs are ignored (only the midpoints are read)
    and only the midpoint of the output is written. -}

foreign import capi safe "arb.h arb_dot_ui" arbDotUI :: ArbT -- ^ res 
  -> ArbT -- ^ s 
  -> CInt -- ^ subtract
  -> ArbVectorT -- ^ x 
  -> CLong -- ^ xstep 
  -> Ptr CULLong -- ^ y
  -> CLong -- ^ ystep
  -> CLong -- ^ len
  -> CLong -- ^ prec 
  -> IO ()
{- ^ Equivalent to :func:`arb_dot`, but with integers in the array @y@.
    The *uiui* and *siui* versions take an array of double-limb integers
    as input; the *siui* version assumes that these represent signed
    integers in two's complement form. -}

foreign import capi safe "arb.h arb_dot_si" arbDotSI :: ArbT -- ^ res 
  -> ArbT -- ^ s 
  -> CInt -- ^ subtract
  -> ArbVectorT -- ^ x 
  -> CLong -- ^ xstep 
  -> Ptr CLLong -- ^ y
  -> CLong -- ^ ystep
  -> CLong -- ^ len
  -> CLong -- ^ prec 
  -> IO ()
{- ^ Equivalent to :func:`arb_dot`, but with integers in the array @y@.
    The *uiui* and *siui* versions take an array of double-limb integers
    as input; the *siui* version assumes that these represent signed
    integers in two's complement form. -}

foreign import capi safe "arb.h arb_dot_uiui" arbDotUIUI :: ArbT -- ^ res 
  -> ArbT -- ^ s 
  -> CInt -- ^ subtract
  -> ArbVectorT -- ^ x 
  -> CLong -- ^ xstep 
  -> Ptr CULLong -- ^ y
  -> CLong -- ^ ystep
  -> CLong -- ^ len
  -> CLong -- ^ prec 
  -> IO ()
{- ^ Equivalent to :func:`arb_dot`, but with integers in the array @y@.
    The *uiui* and *siui* versions take an array of double-limb integers
    as input; the *siui* version assumes that these represent signed
    integers in two's complement form. -}

foreign import capi safe "arb.h arb_dot_siui" arbDotSIUI :: ArbT -- ^ res 
  -> ArbT -- ^ s 
  -> CInt -- ^ subtract
  -> ArbVectorT -- ^ x 
  -> CLong -- ^ xstep 
  -> Ptr CULLong -- ^ y
  -> CLong -- ^ ystep
  -> CLong -- ^ len
  -> CLong -- ^ prec 
  -> IO ()
{- ^ Equivalent to :func:`arb_dot`, but with integers in the array @y@.
    The *uiui* and *siui* versions take an array of double-limb integers
    as input; the *siui* version assumes that these represent signed
    integers in two's complement form. -}

foreign import capi safe "arb.h arb_dot_fmpz" arbDotFmpz :: ArbT -- ^ res 
  -> ArbT -- ^ s 
  -> CInt -- ^ subtract
  -> ArbVectorT -- ^ x 
  -> CLong -- ^ xstep 
  -> FmpzVectorT -- ^ y
  -> CLong -- ^ ystep
  -> CLong -- ^ len
  -> CLong -- ^ prec 
  -> IO ()
{- ^ Equivalent to :func:`arb_dot`, but with integers in the array @y@.
    The *uiui* and *siui* versions take an array of double-limb integers
    as input; the *siui* version assumes that these represent signed
    integers in two's complement form. -}



-------------------------------------------------------------------------------

foreign import capi safe "arb.h arb_sqrt" arbSqrt :: ArbT -- ^ z
  -> ArbT -- ^ x
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets @z@ to the square root of @x@, rounded to @prec@ bits.

    If \(x = m \pm x\) where \(m \ge r \ge 0\), the propagated error is bounded by
    \(\sqrt{m} - \sqrt{m-r} = \sqrt{m} (1 - \sqrt{1 - r/m}) \le \sqrt{m} (r/m + (r/m)^2)/2\). -}

foreign import capi safe "arb.h arb_sqrt_arf" arbSqrtArf :: ArbT -- ^ z
  -> ArfT -- ^ x
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets @z@ to the square root of @x@, rounded to @prec@ bits.

    If \(x = m \pm x\) where \(m \ge r \ge 0\), the propagated error is bounded by
    \(\sqrt{m} - \sqrt{m-r} = \sqrt{m} (1 - \sqrt{1 - r/m}) \le \sqrt{m} (r/m + (r/m)^2)/2\). -}

foreign import capi safe "arb.h arb_sqrt_fmpz" arbSqrtFmpz :: ArbT -- ^ z
  -> FmpzT -- ^ x
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets @z@ to the square root of @x@, rounded to @prec@ bits.

    If \(x = m \pm x\) where \(m \ge r \ge 0\), the propagated error is bounded by
    \(\sqrt{m} - \sqrt{m-r} = \sqrt{m} (1 - \sqrt{1 - r/m}) \le \sqrt{m} (r/m + (r/m)^2)/2\). -}

foreign import capi safe "arb.h arb_sqrt_ui" arbSqrtUI :: ArbT -- ^ z
  -> CULong -- ^ x
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets @z@ to the square root of @x@, rounded to @prec@ bits.

    If \(x = m \pm x\) where \(m \ge r \ge 0\), the propagated error is bounded by
    \(\sqrt{m} - \sqrt{m-r} = \sqrt{m} (1 - \sqrt{1 - r/m}) \le \sqrt{m} (r/m + (r/m)^2)/2\). -}



foreign import capi safe "arb.h arb_sqrtpos" arbSqrtpos :: ArbT -- ^ z
  -> ArbT -- ^ x
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets @z@ to the square root of @x@, assuming that @x@ represents a
    nonnegative number (i.e. discarding any negative numbers in the input
    interval). -}


foreign import capi safe "arb.h arb_hypot" arbHypot :: ArbT -- ^ z
  -> ArbT -- ^ x
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @z@ to \(\sqrt{x^2 + y^2}\).

foreign import capi safe "arb.h arb_rsqrt" arbRsqrt :: ArbT -- ^ z
  -> ArbT -- ^ x
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @z@ to the reciprocal square root of @x@, rounded to @prec@ bits.
-- At high precision, this is faster than computing a square root.

foreign import capi safe "arb.h arb_rsqrt_ui" arbRsqrtUI :: ArbT -- ^ z
  -> CULong -- ^ x
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @z@ to the reciprocal square root of @x@, rounded to @prec@ bits.
-- At high precision, this is faster than computing a square root.

foreign import capi safe "arb.h arb_sqrt1pm1" arbSqrt1PM1 :: ArbT -- ^ z
  -> ArbT -- ^ x
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets \(z = \sqrt{1+x}-1\), computed accurately when \(x \approx 0\).

foreign import capi safe "arb.h arb_root_ui" arbRootUI :: ArbT -- ^ z
  -> ArbT -- ^ x
  -> CULong -- ^ k 
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets @z@ to the @k@-th root of @x@, rounded to @prec@ bits.
    This function selects between different algorithms. For large @k@,
    it evaluates \(\exp(\log(x)/k)\). For small @k@, it uses @arf_root@
    at the midpoint and computes a propagated error bound as follows:
    if input interval is \([m-r, m+r]\) with \(r \le m\), the error is largest at
    \(m-r\) where it satisfies
    \[
        m^{1/k} - (m-r)^{1/k} = m^{1/k} [1 - (1-r/m)^{1/k}]

        = m^{1/k} [1 - \exp(\log(1-r/m)/k)]

        \le m^{1/k} \min(1, -\log(1-r/m)/k)

        = m^{1/k} \min(1, \log(1+r/(m-r))/k).
    \]
    This is evaluated using @mag_log1p@. -}

foreign import capi safe "arb.h arb_root" arbRoot :: ArbT -- ^ z
  -> ArbT -- ^ x
  -> CULong -- ^ k 
  -> CLong -- ^ prec
  -> IO ()
-- ^ Alias for @arb_root_ui@, provided for backwards compatibility.

foreign import capi safe "arb.h arb_sqr" arbSqr :: ArbT -- ^ y
  -> ArbT -- ^ x
  -> CLong -- ^ prec
  -> IO ()
-- ^ Sets @y@ to be the square of @x@.

foreign import capi safe "arb.h arb_pow_fmpz_binexp" arbPowFmpzBinexp :: ArbT -- ^ y
  -> ArbT -- ^ b
  -> FmpzT -- ^ e
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(y = b^e\) using binary exponentiation (with an initial division
    if \(e < 0\)). Provided that @b@ and @e@
    are small enough and the exponent is positive, the exact power can be
    computed by setting the precision to @arf_prec_exact@.

    Note that these functions can get slow if the exponent is
    extremely large (in such cases @arb_pow@ may be superior). -}

foreign import capi safe "arb.h arb_pow_fmpz" arbPowFmpz :: ArbT -- ^ y
  -> ArbT -- ^ b
  -> FmpzT -- ^ e
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(y = b^e\) using binary exponentiation (with an initial division
    if \(e < 0\)). Provided that @b@ and @e@
    are small enough and the exponent is positive, the exact power can be
    computed by setting the precision to @arf_prec_exact@.

    Note that these functions can get slow if the exponent is
    extremely large (in such cases @arb_pow@ may be superior). -}

foreign import capi safe "arb.h arb_pow_ui" arbPowUI :: ArbT -- ^ y
  -> ArbT -- ^ b
  -> CULong -- ^ e
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(y = b^e\) using binary exponentiation (with an initial division
    if \(e < 0\)). Provided that @b@ and @e@
    are small enough and the exponent is positive, the exact power can be
    computed by setting the precision to @arf_prec_exact@.

    Note that these functions can get slow if the exponent is
    extremely large (in such cases @arb_pow@ may be superior). -}

foreign import capi safe "arb.h arb_ui_pow_ui" arbUIPowUI :: ArbT -- ^ y
  -> CULong -- ^ b
  -> CULong -- ^ e
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(y = b^e\) using binary exponentiation (with an initial division
    if \(e < 0\)). Provided that @b@ and @e@
    are small enough and the exponent is positive, the exact power can be
    computed by setting the precision to @arf_prec_exact@.

    Note that these functions can get slow if the exponent is
    extremely large (in such cases @arb_pow@ may be superior). -}

foreign import capi safe "arb.h arb_si_pow_ui" arbSIPowUI :: ArbT -- ^ y
  -> CLong -- ^ b
  -> CULong -- ^ e
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(y = b^e\) using binary exponentiation (with an initial division
    if \(e < 0\)). Provided that @b@ and @e@
    are small enough and the exponent is positive, the exact power can be
    computed by setting the precision to @arf_prec_exact@.

    Note that these functions can get slow if the exponent is
    extremely large (in such cases @arb_pow@ may be superior). -}


foreign import capi safe "arb.h arb_pow_fmpq" arbPowFmpq :: ArbT -- ^ y
  -> ArbT -- ^ x
  -> FmpqT -- ^ a
  -> CLong -- ^ prec
  -> IO ()
{- ^ Sets \(y = b^e\), computed as \(y = (b^{1/q})^p\) if the denominator of
    \(e = p/q\) is small, and generally as \(y = \exp(e \log b)\).

    Note that this function can get slow if the exponent is
    extremely large (in such cases @arb_pow@ may be superior). -}

foreign import capi safe "arb.h arb_pow" arbPow :: ArbT -- ^ z
  -> ArbT -- ^ x
  -> ArbT -- ^ y
  -> CLong -- ^ prec
  -> IO ()
{- Sets \(z = x^y\), computed using binary exponentiation if \(y\) is
    a small exact integer, as \(z = (x^{1/2})^{2y}\) if \(y\) is a small exact
    half-integer, and generally as \(z = \exp(y \log x)\), except giving the
    obvious finite result if \(x\) is \(a \pm a\) and \(y\) is positive. -}