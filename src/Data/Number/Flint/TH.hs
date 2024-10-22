{-# LANGUAGE TemplateHaskell #-}
module Data.Number.Flint.TH (
    curryN,
    genCurries,
    cType2HaskellType,
    CFunction(..),
    generateFFIBindings,
    cFunctions,
    Parameter(..)
) where


import Control.Monad ( replicateM, forM )
import Text.Casing ( fromSnake, toCamel )
import Text.Pandoc(Pandoc(..), runIOorExplode, readRST, def, writeHaddock)
import qualified Data.Text.IO as TIO

import Language.Haskell.TH
    ( mkName,
      Quote(newName),
      Exp(VarE, TupE, LamE, AppE),
      Clause(Clause),
      Q,
      Pat(VarP),
      Type(ConT, ArrowT, AppT),
      Dec(ForeignD, FunD),
      Name,
      lookupTypeName,
      Body(NormalB),
      Callconv(CApi),
      Foreign(ImportF),
      Safety(Unsafe) )

{-
curryN :: String -> Q Exp
curryN name_ = 
    let name = (toCamel . fromSnake) name_
    in do 
      n  <- newName name
  -}

processRST :: String -> IO ()
processRST rstFile = do
  text <- TIO.readFile rstFile
  Pandoc b ps <- runIOorExplode $ readRST def text
  hd <- runIOorExplode $ writeHaddock def $ Pandoc b (take 15 ps)
  print $ Pandoc b (take 15 ps)

curryN :: Int -> Q Exp
curryN n = do
  f  <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map (Just . VarE) xs)
  return $ LamE args (AppE (VarE f) ntup)

genCurries :: Int -> Q [Dec]
genCurries n = forM [1..n] mkCurryDec
  where mkCurryDec ith = do
          cury <- curryN ith
          let name = mkName $ "curry" ++ show ith
          return $ FunD name [Clause [] (NormalB cury) []]


data Parameter = Parameter {
  parameterType :: String,
  parameterName :: String
}

data CFunction = CFunction {
  header :: String,
  returnType :: String,
  functionName :: String,
  parameters :: [Parameter]
}

cType2HaskellType :: String -> Q (Maybe Name)
cType2HaskellType "int" = lookupTypeName "CInt"
cType2HaskellType "ulong" = lookupTypeName "CULong"
cType2HaskellType "slong" = lookupTypeName "CLong"
cType2HaskellType "arb_t" = lookupTypeName "ArbT"
cType2HaskellType a = error ("unsupported c-type " ++ a)


-- foreign import capi safe "arb.h arb_new" arbNew :: IO ArbT
cFunctions :: [CFunction]
cFunctions = [
  CFunction {
    header = "arb.h",
    returnType = "arb_t",
    functionName = "arb_new",
    parameters = [
    ]
  }]

snakeCase2CamlCase :: String -> String
snakeCase2CamlCase = toCamel . fromSnake

convertType :: CFunction -> Q Type
convertType cf =
  let ps = parameters cf
      rt = returnType cf
  in do
    Just rtName <- cType2HaskellType rt
    Just ioName <- lookupTypeName "IO"
    convertType' ps (AppT (ConT ioName) (ConT rtName))
  where convertType' [] t = return t
        convertType' (p:ps) t = do
          Just r <- (cType2HaskellType . parameterType) p
          convertType' ps (AppT (AppT ArrowT (ConT r)) t)

-- Function to generate FFI bindings from a list of function names and types
generateFFIBindings :: [CFunction] -> Q [Dec]
generateFFIBindings = fmap concat . mapM (\cf -> do
    let hsName = (mkName . snakeCase2CamlCase . functionName) cf
    let str = header cf ++ " " ++ functionName cf
    typ <- convertType cf
    return [ForeignD (ImportF CApi Unsafe str hsName typ )])
