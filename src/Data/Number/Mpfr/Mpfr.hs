{-# LANGUAGE GADTs #-}

module Data.Number.Mpfr.Mpfr (
  MpfrC,
  MpfrRndC (..),
  MpfrT (..),
  mpfr_rnda,
  mpfr_rndd,
  mpfr_rndn,
  mpfr_rndu,
  mpfr_rndz,
  mpfr_rndf,
) where

import Foreign.C.Types (CLong (..))
import Foreign.Ptr

data MpfrC

newtype MpfrT where
  MpfrT :: {_MpfrT :: Ptr MpfrC} -> MpfrT

{- | Note: In particular for a 1-digit precision (in radix 2 or other radices, as in conversions
to a string of digits), one considers the significands associated with the exponent of the number
to be rounded. For instance, to round the number 95 in radix 10 with a 1-digit precision, one considers
its truncated 1-digit integer significand 9 and the following integer 10 (since these are consecutive
integers, exactly one of them is even). 10 is the even significand, so that 95 will be rounded to
100, not to 90.

For the directed rounding modes, a number @x@ is rounded to the number @y@ that is the closest to @x@ such that

* mpfr_rndd: @y@ is less than or equal to @x@;
* mpfr_rndu: @y@ is greater than or equal to @x@;
* mpfr_rndz: abs(y) is less than or equal to abs(x);
* mpfr_rnda: abs(y) is greater than or equal to abs(x).

The mpfr_rndf mode works as follows: the computed value is
either that corresponding to mpfr_rndd or that corresponding to mpfr_rndu.
In particular when those values are identical, i.e., when the result of
the corresponding operation is exactly representable, that exact result
is returned. Thus, the computed result can take at most two possible values,
and in absence of underflow/overflow, the corresponding error is strictly
less than one ulp (unit in the last place) of that result and of the exact
result. For mpfr_rndf, the ternary value (defined below) and the inexact
flag (defined later, as with the other flags) are unspecified, the
divide-by-zero flag is as with other roundings, and the underflow and overflow
flags match what would be obtained in the case the computed value is the same
as with mpfr_rndd ormpfr_rndu. The results may not be reproducible.

Most MPFR functions take as first argument the destination variable, as second and
following arguments the input variables, as last argument a rounding mode, and have
a return value of type int, called the ternary value. The value stored in the
destination variable is correctly rounded, i.e., MPFR behaves as if it computed the
result with an infinite precision, then rounded it to the precision of this variable.
The input variables are regarded as exact (in particular, their precision does not affect the result).

As a consequence, in case of a non-zero real rounded result, the error on the result is less
than or equal to 1/2 ulp (unit in the last place) of that result in the rounding to nearest
mode, and less than 1 ulp of that result in the directed rounding modes (a ulp is the weight
of the least significant represented bit of the result after rounding).

Unless documented otherwise, functions returning an int return a ternary value. If the
ternary value is zero, it means that the value stored in the destination variable is
the exact result of the corresponding mathematical function. If the ternary value is
positive (resp. negative), it means the value stored in the destination variable is
greater (resp. lower) than the exact result. For example with the mpfr_rndu rounding mode,
the ternary value is usually positive, except when the result is exact, in which case it
is zero. In the case of an infinite result, it is considered as inexact when it was obtained
by overflow, and exact otherwise. A NaN result (Not-a-Number) always corresponds to an exact
return value. The opposite of a returned ternary value is guaranteed to be
representable in an int.

Unless documented otherwise, functions returning as result the value 1 (or any
other value specified in this manual) for special cases (like acos(0)) yield an
overflow or an underflow if that value is not representable in the current
exponent range.
-}
newtype MpfrRndC where
  MpfrRndC :: {_MpfrRndC :: CLong} -> MpfrRndC
  deriving (Show, Eq)

{- | round to nearest, with the even rounding rule (roundTiesToEven in IEEE 754); see details below.

The @mpfr_rndn@ mode works like @roundTiesToEven@ from the IEEE 754 standard: in case the
number to be rounded lies exactly in the middle between two consecutive representable
numbers, it is rounded to the one with an even significand; in radix 2, this means that
the least significant bit is 0. For example, the number 2.5, which is represented by (10.1) in
binary, is rounded to (10.0) = 2 with a precision of two bits, and not to (11.0) = 3. This
rule avoids the drift phenomenon mentioned by Knuth in volume 2 of The Art of Computer
Programming (Section 4.2.2).
-}
foreign import capi safe "mpfr.h value MPFR_RNDN" mpfr_rndn :: MpfrRndC

-- | round toward negative infinity (roundTowardNegative in IEEE 754).
foreign import capi safe "mpfr.h value MPFR_RNDD" mpfr_rndd :: MpfrRndC

-- | round toward positive infinity (roundTowardPositive in IEEE 754).
foreign import capi safe "mpfr.h value MPFR_RNDU" mpfr_rndu :: MpfrRndC

-- | round toward zero (roundTowardZero in IEEE 754).
foreign import capi safe "mpfr.h value MPFR_RNDZ" mpfr_rndz :: MpfrRndC

-- | round away from zero.
foreign import capi safe "mpfr.h value MPFR_RNDA" mpfr_rnda :: MpfrRndC

{- | faithful rounding. This feature is currently experimental. Specific support for
this rounding mode has been added to some functions, such as the basic operations
(addition, subtraction, multiplication, square, division, square root) or when explicitly
documented. It might also work with other functions, as it is possible that they do not
need modification in their code; even though a correct behavior is not guaranteed yet
(corrections were done when failures occurred in the test suite, but almost nothing has
been checked manually), failures should be regarded as bugs and reported, so that they
can be fixed.
-}
foreign import capi safe "mpfr.h value MPFR_RNDF" mpfr_rndf :: MpfrRndC