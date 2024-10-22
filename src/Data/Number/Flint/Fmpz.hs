{-# LANGUAGE GADTs #-}

module Data.Number.Flint.Fmpz (
  FmpzT (..),
  FmpzVectorT (..)
) where

import Foreign.Ptr (Ptr)

data FmpzC

newtype FmpzT where
  FmpzT :: {_FmpzT :: Ptr FmpzC} -> FmpzT

data FmpzVectorC

newtype FmpzVectorT where
  FmpzVectorT :: {_FmpzVectorT :: Ptr FmpzVectorC} -> FmpzVectorT