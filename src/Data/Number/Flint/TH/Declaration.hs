{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeData #-}
module Data.Number.Flint.TH.Declaration (
    TypeConversionData(..)
  , TypeData(..)
  , describesFunctionType
  , typeConversionData
  , cTypeToHaskellType
  , runTypeConversion
  , isAnnonymousType
  , enumInTypeSignature
  , isPtrCType
  , isSimpleCType
) where
import qualified Language.C as CP
import qualified Language.C.Analysis as Ast
import Data.Number.Flint.TH.Error ( ErrorString(..) )
import Control.Monad.State
    ( MonadState(get, put), MonadTrans(lift), StateT(runStateT) )
import Data.Number.Flint.TH.TreeWalker(TreeWalker(..))
import qualified Language.C.Data.Ident as Ast

import qualified Language.C as Ast
import Control.Monad.Error.Class ( MonadError(throwError) )
import Data.Functor ((<&>))


data TypeData = SimpleType String
              | EnumType String
              | CompType String
              | PtrType TypeData
              | ArrayType TypeData
              | FunctionType TypeData [TypeData]
              | TypeDefType String TypeData

data TypeConversionData = TypeConversionData {
    includesEnums :: Bool
  , currentNodeInfo :: Maybe CP.NodeInfo
  , convertedTypes :: Maybe TypeData
  , ty :: Ast.Type
}


typeConversionData :: (TreeWalker m) => Ast.Type -> m TypeConversionData
typeConversionData t = do
  nodeInfo <- getNodeInfo
  return $ TypeConversionData { includesEnums = False, currentNodeInfo = nodeInfo, convertedTypes = Nothing, ty = t}

type TypeConversionMonad = StateT TypeConversionData (Either ErrorString)

newTypeConversionData :: Ast.Type -> TypeConversionMonad TypeConversionData
newTypeConversionData t = do
    s <- get
    return TypeConversionData{includesEnums = False, currentNodeInfo = currentNodeInfo s, convertedTypes = Nothing, ty = t}

describesFunctionType :: TypeConversionData -> Bool
describesFunctionType t = (length . convertedTypes) t > 1


runTypeConversion :: TypeConversionData -> TypeConversionMonad a -> Either ErrorString TypeConversionData
runTypeConversion t f =
  case runStateT f t of
    Left e -> Left e
    Right (_, b) -> Right b

enumInTypeSignature :: TypeConversionMonad ()
enumInTypeSignature = do
  s <- get
  put s{includesEnums=True}


setConvertedType :: (a -> TypeData) -> Maybe a -> TypeConversionMonad ()
setConvertedType f a = do
  s <- get
  case a of
    Nothing -> put s{convertedTypes=Nothing}
    Just b -> put s{convertedTypes= Just $ f b}

getType :: TypeConversionMonad Ast.Type
getType = do
  s <- get
  (return . ty) s

cTypeToHaskellType :: TypeConversionMonad ()
cTypeToHaskellType =  do
  t <- getType
  case t of
    (Ast.DirectType tyName _ _) -> do
      (f, name) <- cTypeNameToHaskell tyName
      setConvertedType f $ Just name
    (Ast.PtrType ty2 _ _) -> do
        tty <- newTypeConversionData ty2
        q <- lift $ runTypeConversion tty cTypeToHaskellType
        setConvertedType PtrType $ convertedTypes q
    (Ast.ArrayType ty2 _ _ _) -> do
      tty <- newTypeConversionData ty2
      q <- lift $ runTypeConversion tty cTypeToHaskellType
      setConvertedType ArrayType $ convertedTypes q
    (Ast.FunctionType (Ast.FunType ty2 params _) _) -> do
      tty <- newTypeConversionData ty2
      q <- lift $ runTypeConversion tty cTypeToHaskellType
      ps <- ((mapM functionParamToHaskellType . reverse) params >>= mapM (return . convertedTypes)) <&> sequence
      case convertedTypes q of
        Nothing -> throwError . ErrorString $ "FunctionType without return type in cTypeToHaskellType."
        Just w -> setConvertedType (FunctionType w) ps
    (Ast.TypeDefType (Ast.TypeDefRef (Ast.Ident name _ _) ty2 _) _ _) -> do
      tty <- newTypeConversionData ty2
      q <- lift $ runTypeConversion tty cTypeToHaskellType
      setConvertedType (TypeDefType name) $ convertedTypes q
    t2 -> throwError . ErrorString $ "Unhandled type in cTypeToHaskellType: " ++ show t2

functionParamToHaskellType :: Ast.ParamDecl -> TypeConversionMonad TypeConversionData
functionParamToHaskellType (Ast.ParamDecl (Ast.VarDecl _ _ t2) _) = do
  tty <- newTypeConversionData t2
  lift $ runTypeConversion tty cTypeToHaskellType

functionParamToHaskellType (Ast.AbstractParamDecl varDecl nodeInfo) =
  functionParamToHaskellType (Ast.ParamDecl varDecl nodeInfo)




isPtrCType :: Ast.Type -> Bool
isPtrCType (Ast.PtrType {}) = True
isPtrCType (Ast.ArrayType{}) = True
isPtrCType _ = False

isAnnonymousType :: Ast.Type -> Bool
isAnnonymousType (Ast.DirectType (Ast.TyComp (Ast.CompTypeRef (Ast.AnonymousRef _) _ _)) _ _) = True
isAnnonymousType (Ast.DirectType (Ast.TyEnum (Ast.EnumTypeRef (Ast.AnonymousRef _) _ )) _ _)  = True
isAnnonymousType _ = False

isSimpleCType :: Ast.Type -> Bool
isSimpleCType (Ast.DirectType t _ _) = isSimpleTypeName t
isSimpleCType (Ast.PtrType t _ _) = isSimpleCType t
isSimpleCType (Ast.ArrayType t _ _ _) = isSimpleCType t
isSimpleCType _ = False


cIntegralTypeNameToHaskell :: Ast.IntType -> TypeConversionMonad String
cIntegralTypeNameToHaskell Ast.TyBool    = return "CBool"
cIntegralTypeNameToHaskell Ast.TyChar    = return "CChar"
cIntegralTypeNameToHaskell Ast.TySChar   = return "CSChar"
cIntegralTypeNameToHaskell Ast.TyUChar   = return "CUChar"
cIntegralTypeNameToHaskell Ast.TyShort   = return "CShort"
cIntegralTypeNameToHaskell Ast.TyUShort  = return "CUShort"
cIntegralTypeNameToHaskell Ast.TyInt     = return "CInt"
cIntegralTypeNameToHaskell Ast.TyUInt    = return "CUInt"
cIntegralTypeNameToHaskell Ast.TyLong    = return "CLong"
cIntegralTypeNameToHaskell Ast.TyULong   = return "CULong"
cIntegralTypeNameToHaskell Ast.TyLLong   = return "CLLong"
cIntegralTypeNameToHaskell Ast.TyULLong  = return "CULLong"
cIntegralTypeNameToHaskell Ast.TyInt128  = throwError $ ErrorString "128 bit integers are not supported"
cIntegralTypeNameToHaskell Ast.TyUInt128 = throwError $ ErrorString "128 bit integers are not supported"

cFloatTypeNameToHaskell :: Ast.FloatType -> TypeConversionMonad String
cFloatTypeNameToHaskell Ast.TyFloat        = return "CFloat"
cFloatTypeNameToHaskell Ast.TyDouble       = return "CDouble"
cFloatTypeNameToHaskell Ast.TyLDouble      = throwError $ ErrorString "Long Double is not supported"
cFloatTypeNameToHaskell (Ast.TyFloatN _ _) = throwError $ ErrorString "floatN is not supported"

isSimpleTypeName :: Ast.TypeName -> Bool
isSimpleTypeName Ast.TyVoid = True
isSimpleTypeName (Ast.TyIntegral _) = True
isSimpleTypeName (Ast.TyFloating _ ) = True
isSimpleTypeName _ = False

cTypeNameToHaskell :: Ast.TypeName -> TypeConversionMonad (String -> TypeData, String)
cTypeNameToHaskell Ast.TyVoid         = return (SimpleType, "()")
cTypeNameToHaskell (Ast.TyIntegral t) = cIntegralTypeNameToHaskell t <&> (SimpleType,)
cTypeNameToHaskell (Ast.TyFloating t) = cFloatTypeNameToHaskell t <&> (SimpleType,)
cTypeNameToHaskell (Ast.TyComp (Ast.CompTypeRef (Ast.NamedRef (Ast.Ident name _ _)) _ _))  = return (CompType, name)
cTypeNameToHaskell (Ast.TyComp (Ast.CompTypeRef (Ast.AnonymousRef (Ast.Name nameId)) _ _)) = return (CompType, "UnnamedNo" ++ show nameId)
cTypeNameToHaskell (Ast.TyEnum (Ast.EnumTypeRef (Ast.NamedRef (Ast.Ident name _ _)) _ ))   = return (EnumType, name)
cTypeNameToHaskell (Ast.TyEnum (Ast.EnumTypeRef (Ast.AnonymousRef (Ast.Name nameId)) _ ))  = return (EnumType, "UnnamedEnumNo" ++ show nameId)
cTypeNameToHaskell t                  = throwError . ErrorString $ "unsupported type '" ++ show t ++ "' in cTypeNameToHaskell"