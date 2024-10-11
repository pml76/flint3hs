{-# LANGUAGE GADTs #-}
module Data.Number.Flint.Flint (
    FlintRandC
  , FlintRandT(..)
) where 

import Foreign.Ptr ( Ptr )

data FlintRandC

newtype FlintRandT where
    FlintRandT :: {_FlintRandT :: Ptr FlintRandC} -> FlintRandT