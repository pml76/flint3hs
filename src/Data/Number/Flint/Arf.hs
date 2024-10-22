{-# LANGUAGE GADTs #-}

module Data.Number.Flint.Arf (
  ArfC,
  ArfT (..),
  ArfRndC (..),
  arf_rnd_up,
  arf_rnd_down,
  arf_rnd_floor,
  arf_rnd_ceil,
  arf_rnd_near,
  arf_prec_exact,
) where

import Foreign.C.Types (CLLong (..))
import Foreign.Ptr (Ptr)

data ArfC

newtype ArfT where
  ArfT :: {_ArfT :: Ptr ArfC} -> ArfT

newtype ArfRndC where
  ArfRndC :: {_ArfRndC :: CLLong} -> ArfRndC
  deriving (Show, Eq)

{- | Specifies that the result of an operation should be rounded to
the nearest representable number in the direction towards zero.
-}
foreign import capi safe "flint/arf.h value ARF_RND_UP" arf_rnd_up :: ArfRndC --     = ArfRndC #const ARF_RND_UP

{- | Specifies that the result of an operation should be rounded to
the nearest representable number in the direction away from zero.
-}
foreign import capi safe "flint/arf.h value ARF_RND_DOWN" arf_rnd_down :: ArfRndC -- = ArfRndC #const ARF_RND_DOWN

{- | Specifies that the result of an operation should be rounded to
the nearest representable number in the direction towards minus
infinity.
-}
foreign import capi safe "flint/arf.h value ARF_RND_FLOOR" arf_rnd_floor :: ArfRndC

{- | Specifies that the result of an operation should be rounded to
the nearest representable number in the direction towards plus
infinity.
-}
foreign import capi safe "flint/arf.h value ARF_RND_CEIL" arf_rnd_ceil :: ArfRndC

{- | Specifies that the result of an operation should be rounded to
the nearest representable number, rounding to even if there is a
tie between two values.
-}
foreign import capi safe "flint/arf.h value ARF_RND_NEAR" arf_rnd_near :: ArfRndC

{- | If passed as the precision parameter to a function, indicates
that no rounding is to be performed. __Warning__: use of this value
is unsafe in general. It must only be passed as input under the
following two conditions:

 * The operation in question can inherently be viewed as an exact operation
   in \(\mathbb{Z}[\tfrac{1}{2}]\) for all possible inputs, provided that
   the precision is large enough. Examples include addition,
   multiplication, conversion from integer types to arbitrary-precision
   floating-point types, and evaluation of some integer-valued functions.

 * The exact result of the operation will certainly fit in memory.
   Note that, for example, adding two numbers whose exponents are far
   apart can easily produce an exact result that is far too large to
   store in memory.

 The typical use case is to work with small integer values, double
 precision constants, and the like. It is also useful when writing
 test code. If in doubt, simply try with some convenient high precision
 instead of using this special value, and check that the result is exact.
-}
foreign import capi safe "flint/arf.h value ARF_PREC_EXACT" arf_prec_exact :: ArfRndC