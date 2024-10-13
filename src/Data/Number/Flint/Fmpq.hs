{-# LANGUAGE GADTs #-}

module Data.Number.Flint.Fmpq (
  FmpqC,
  FmpqT (..),
) where

import Foreign.Ptr

data FmpqC

newtype FmpqT where
  FmpqT :: {_FmpqT :: Ptr FmpqC} -> FmpqT