{-# LANGUAGE GADTs #-}
module Data.Number.Flint.Fmpz (
    FmpzC
  , FmpzT(..)
) where

import Foreign.Ptr ( Ptr )

data FmpzC

newtype FmpzT where
    FmpzT :: {_FmpzT :: Ptr FmpzC} -> FmpzT