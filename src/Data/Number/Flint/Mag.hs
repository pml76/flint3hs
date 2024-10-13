{-# LANGUAGE GADTs #-}

module Data.Number.Flint.Mag (
  MagC,
  MagT (..),
) where

import Foreign.Ptr

data MagC

newtype MagT where
  MagT :: {_MagT :: Ptr MagC} -> MagT