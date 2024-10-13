{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Number.Flint.FlintVariable (
  FlintVariable (..),
  NewFlintVariable (..),
  NewVectorFlintVariable (..),
  VectorLength (..),
) where

import Foreign.C.Types (CDouble, CInt, CLong, CUInt, CULong)

class FlintVariable a c | a -> c, c -> a where
  withFlintVariable :: (c -> IO b) -> a -> IO b

  with2FlintVariables :: (FlintVariable a2 c2) => (c -> c2 -> IO b) -> a -> a2 -> IO b
  with2FlintVariables f a b = do
    g <- withFlintVariable (return . f) a
    withFlintVariable g b

  with3FlintVariables :: (FlintVariable a1 c1, FlintVariable a2 c2) => (c -> c1 -> c2 -> IO b) -> a -> a1 -> a2 -> IO b
  with3FlintVariables f a b c = do
    g <- withFlintVariable (return . f) a
    with2FlintVariables g b c

  with4FlintVariables :: (FlintVariable a2 c2, FlintVariable a3 c3, FlintVariable a4 c4) => (c -> c2 -> c3 -> c4 -> IO b) -> a -> a2 -> a3 -> a4 -> IO b
  with4FlintVariables f a b c d = do
    g <- withFlintVariable (return . f) a
    with3FlintVariables g b c d

  with5FlintVariables :: (FlintVariable a1 c1, FlintVariable a2 c2, FlintVariable a3 c3, FlintVariable a4 c4) => (c -> c1 -> c2 -> c3 -> c4 -> IO b) -> a -> a1 -> a2 -> a3 -> a4 -> IO b
  with5FlintVariables f a b c d e = do
    g <- withFlintVariable (return . f) a
    with4FlintVariables g b c d e

instance FlintVariable CLong CLong where
  withFlintVariable :: (CLong -> IO b) -> CLong -> IO b
  withFlintVariable f = f

instance FlintVariable CULong CULong where
  withFlintVariable :: (CULong -> IO b) -> CULong -> IO b
  withFlintVariable f = f

instance FlintVariable CDouble CDouble where
  withFlintVariable :: (CDouble -> IO b) -> CDouble -> IO b
  withFlintVariable f = f

instance FlintVariable CInt CInt where
  withFlintVariable :: (CInt -> IO b) -> CInt -> IO b
  withFlintVariable f = f

instance FlintVariable CUInt CUInt where
  withFlintVariable :: (CUInt -> IO b) -> CUInt -> IO b
  withFlintVariable f = f

class NewFlintVariable a c | a -> c, c -> a where
  createNewFlintVariable :: IO a

  withNewFlintVariable :: (FlintVariable a c) => (c -> IO a) -> IO a
  withNewFlintVariable f = createNewFlintVariable >>= withFlintVariable f

  withNewFlintVariable2 :: (FlintVariable a c, FlintVariable a1 c1) => (c -> c1 -> IO a) -> a1 -> IO a
  withNewFlintVariable2 f a1 = do
    a <- createNewFlintVariable
    with2FlintVariables f a a1

  withNewFlintVariable3 :: (FlintVariable a c, FlintVariable a1 c1, FlintVariable a2 c2) => (c -> c1 -> c2 -> IO a) -> a1 -> a2 -> IO a
  withNewFlintVariable3 f a1 a2 = do
    a <- createNewFlintVariable
    with3FlintVariables f a a1 a2

  withNewFlintVariable4 :: (FlintVariable a c, FlintVariable a1 c1, FlintVariable a2 c2, FlintVariable a3 c3) => (c -> c1 -> c2 -> c3 -> IO a) -> a1 -> a2 -> a3 -> IO a
  withNewFlintVariable4 f a1 a2 a3 = do
    a <- createNewFlintVariable
    with4FlintVariables f a a1 a2 a3

  withNewFlintVariable5 :: (FlintVariable a c, FlintVariable a1 c1, FlintVariable a2 c2, FlintVariable a3 c3, FlintVariable a4 c4) => (c -> c1 -> c2 -> c3 -> c4 -> IO a) -> a1 -> a2 -> a3 -> a4 -> IO a
  withNewFlintVariable5 f a1 a2 a3 a4 = do
    a <- createNewFlintVariable
    with5FlintVariables f a a1 a2 a3 a4

newtype VectorLength where
  VectorLength :: {_VectorLength :: CULong} -> VectorLength

-- TODO: check if it is possible to create a monad to take care of the lengths of the vectors
-- aiming at something like: withLength length $ complicatedVectorCalculation

-- TODO: If possible implement same thing for precision. Aiming at:
-- withPrecision prec $ calculation

class NewVectorFlintVariable a c | a -> c, c -> a where
  createNewVectorFlintVariable :: VectorLength -> IO a

  withNewVectorFlintVariable :: (FlintVariable a c) => (c -> IO a) -> VectorLength -> IO a
  withNewVectorFlintVariable f n = createNewVectorFlintVariable n >>= withFlintVariable f

{-  withNewVectorFlintVariable2 :: (FlintVariable a c, FlintVariable a1 c1 ) => CULong -> (c -> c1 -> IO a) -> a1 -> IO a
  withNewVectorFlintVariable2 n f a1 = do
    a <- createNewVectorFlintVariable n
    with2FlintVariables f a a1

  withNewVectorFlintVariable3 :: (FlintVariable a c, FlintVariable a1 c1, FlintVariable a2 c2) => CULong -> (c -> c1 -> c2 -> IO a) -> a1 -> a2 -> IO a
  withNewVectorFlintVariable3 n f a1 a2 = do
    a <- createNewVectorFlintVariable n
    with3FlintVariables f a a1 a2

  withNewVectorFlintVariable4 :: (FlintVariable a c, FlintVariable a1 c1, FlintVariable a2 c2, FlintVariable a3 c3) => CULong -> (c -> c1 -> c2 -> c3 -> IO a) -> a1 -> a2 -> a3 -> IO a
  withNewVectorFlintVariable4 n f a1 a2 a3 = do
    a <- createNewVectorFlintVariable n
    with4FlintVariables f a a1 a2 a3

  withNewVectorFlintVariable5 :: (FlintVariable a c, FlintVariable a1 c1, FlintVariable a2 c2, FlintVariable a3 c3, FlintVariable a4 c4) => CULong -> (c -> c1 -> c2 -> c3 -> c4 -> IO a) -> a1 -> a2 -> a3 -> a4 -> IO a
  withNewVectorFlintVariable5 n f a1 a2 a3 a4 = do
    a <- createNewVectorFlintVariable n
    with5FlintVariables f a a1 a2 a3 a4
  -}
