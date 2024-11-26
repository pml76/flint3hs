{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Number.Flint.TH.CodeMonad (
  CodeData (..),
  TreeWalker (..),
  CodeMonad,
  codeData,
  runCodeMonad,
  walkObjects,
) where

import Control.Monad (foldM_, (>=>))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (
  MonadState (get, put),
  MonadTrans (lift),
  StateT (runStateT),
  gets,
 )
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Data.Map
import Data.Maybe (fromMaybe)
import qualified Data.Number.Flint.TH.Declaration as Declaration
import Data.Number.Flint.TH.Error (ErrorString (..))
import Data.Number.Flint.TH.TreeWalker (TreeWalker (..))
import qualified Language.C as Ast
import qualified Language.C.Analysis as Ast
import qualified Language.C.Data.Ident as Ast
import Text.Casing (camel, pascal)
import Data.Number.Flint.TH.Declaration (SourcePair(cLanguage))
import Data.Foldable (foldlM)

data CodeData = CodeData
  { haskellCodeLinesOutput :: [String]
  , cCodeLinesOutput :: [String]
  , currentNodeInfo :: Maybe Ast.NodeInfo
  , globalDeclarations :: Ast.GlobalDecls
  , filesToConsider :: [String]
  , outputHaskellFile :: String 
  , outputCFile :: String
  }

codeData :: [String] -> String -> String -> Ast.GlobalDecls -> CodeData
codeData files oHaskellFile oCFile gDecls =
  CodeData
    { haskellCodeLinesOutput = []
    , cCodeLinesOutput = []
    , currentNodeInfo = Nothing
    , globalDeclarations = gDecls
    , filesToConsider = files
    , outputHaskellFile = oHaskellFile
    , outputCFile = oCFile
    }

type CodeMonad = StateT CodeData (Either ErrorString)

runCodeMonad :: CodeData -> CodeMonad a -> Either ErrorString CodeData
runCodeMonad cd f =
  let a = runStateT f cd
   in case a of
        Left e -> Left e
        Right (_, b) -> Right b

instance (TreeWalker CodeMonad) where
  getNodeInfo :: CodeMonad (Maybe Ast.NodeInfo)
  getNodeInfo = do
    s <- get
    (return . currentNodeInfo) s

{- | enum values will be reported as declarations. We need to filter them out since they
are handled in another way.
-}
notIsEnumValue :: Ast.IdentDecl -> Bool
notIsEnumValue = notIsEnumValue' . Ast.getVarDecl
 where
  notIsEnumValue' (Ast.VarDecl _ _ ty) = notIsEnumValue'' ty
  notIsEnumValue'' (Ast.DirectType (Ast.TyEnum _) _ _) = False
  notIsEnumValue'' _ = True

walkObjects :: CodeMonad ()
walkObjects = do
  typeDefs <- getTypeDefs
  mapM_ (processTypeDef_ . snd) $ Data.Map.toList typeDefs
  objs <- getDeclarations
  mapM_ (processIdentDecl_ . snd) . filter (notIsEnumValue . snd) $ Data.Map.toList objs
 where
  processIdentDecl_ decl = setNodeInfo (Ast.nodeInfo decl) >> (processVarDecl . Ast.getVarDecl) decl
  processTypeDef_ typeDef = do
    setNodeInfo (Ast.nodeInfo typeDef)
    processTypeDef typeDef

getDeclarations :: CodeMonad (Data.Map.Map Ast.Ident Ast.IdentDecl)
getDeclarations = do
  cd <- get
  return . Data.Map.filter (nodeStemsFromFilesToConsider $ filesToConsider cd) $ Ast.gObjs (globalDeclarations cd)

getTags :: CodeMonad (Data.Map.Map Ast.SUERef Ast.TagDef)
getTags = do
  cd <- get
  return . Data.Map.filter (nodeStemsFromFilesToConsider $ filesToConsider cd) $ Ast.gTags (globalDeclarations cd)

getTypeDefs :: CodeMonad (Data.Map.Map Ast.Ident Ast.TypeDef)
getTypeDefs = do
  cd <- get
  return . Data.Map.filter (nodeStemsFromFilesToConsider $ filesToConsider cd) $ Ast.gTypeDefs (globalDeclarations cd)

setNodeInfo :: Ast.NodeInfo -> CodeMonad ()
setNodeInfo n = do
  s <- get
  put (s{currentNodeInfo = Just n})

getFileName :: CodeMonad (Maybe FilePath)
getFileName = gets (currentNodeInfo >=> Ast.fileOfNode)

addHaskellCodeLine :: String -> CodeMonad ()
addHaskellCodeLine str = do
  s <- get
  put s{haskellCodeLinesOutput = str : haskellCodeLinesOutput s}

addCCodeLine :: String -> CodeMonad ()
addCCodeLine str = do
  s <- get
  put s{cCodeLinesOutput = str : cCodeLinesOutput s}

processTag :: Ast.SUERef -> Ast.TagDef -> CodeMonad ()
processTag (Ast.NamedRef (Ast.Ident name _ _)) (Ast.EnumDef (Ast.EnumType _ enumerators _ _)) = do
  let cNames = map (\(Ast.Enumerator (Ast.Ident enumeratorName _ _) _ _ _) -> enumeratorName) enumerators
      haskellTypeConstructors = map (\(Ast.Enumerator (Ast.Ident enumeratorName _ _) _ _ _) -> pascal enumeratorName) enumerators
      haskellType = "data " ++ pascal name ++ " = " ++ intercalate " | " haskellTypeConstructors
  addHaskellCodeLine haskellType
  addHaskellCodeLine $ "instance Enum " ++ pascal name ++ " where"
  foldM_
    ( \i s -> do
        addHaskellCodeLine $ "  fromEnum " ++ s ++ " = " ++ show i
        return $ i + 1
    )
    (0 :: Int)
    haskellTypeConstructors
  foldM_
    ( \i s -> do
        addHaskellCodeLine $ "  toEnum " ++ show i ++ " = " ++ s
        return $ i + 1
    )
    (0 :: Int)
    haskellTypeConstructors
  addCCodeLine $ name ++ " intTo" ++ pascal name ++ "(unsigned long p) {"
  foldM_
    ( \i s -> do
        addCCodeLine $ "  if( p == " ++ show i ++ ") return " ++ s ++ ";"
        return $ i + 1
    )
    (0 :: Int)
    cNames
  addCCodeLine "}"
  addCCodeLine $ "unsigned long " ++ camel name ++ "ToInt(" ++ name ++ " p) {"
  foldM_
    ( \i s -> do
        addCCodeLine $ "  if( p == " ++ s ++ ") return " ++ show i ++ ";"
        return $ i + 1
    )
    (0 :: Int)
    cNames
  addCCodeLine "}"
processTag s t = throwError . ErrorString $ "Unknown combination of SUERef '" ++ show s ++ "'and TagDef '" ++ show t ++ "'."

typeDataToEnumMarshallHaskellFunctionDefiniton :: String -> Declaration.TypeConversionData -> CodeMonad ()
typeDataToEnumMarshallHaskellFunctionDefiniton name tcd =
  case Declaration.convertedTypes tcd of
    Nothing -> throwError . ErrorString $ "Cannot compose a marshalling Haskell function from Nothing"
    Just t -> typeData2EnumMarshallHaskellFunctionDefintion
      where typeData2EnumMarshallHaskellFunctionDefintion :: CodeMonad ()
            typeData2EnumMarshallHaskellFunctionDefintion = do
              case t of
                (Declaration.FunctionType ret params) -> do
                  typeSignature <- typeData2HaskellTypeSignature False tcd
                  addHaskellCodeLine $ name ++ " :: " ++ typeSignature
                  let mod_ret = if Declaration.isEnumType ret then "fmap (toEnum . fromIntegral) $ " else ""
                  addHaskellCodeLine $ name ++ snd (foldr (\a (i,s) -> (i+1,s ++ a ++ show i)) (0::Int, "") (replicate (length params) " a"))
                                            ++ " = " ++ mod_ret ++ name ++ "_" ++ snd (foldr (\a (i,s) ->
                                              (i+1, s ++ if Declaration.isEnumType a then " (fromIntegral . fromEnum $ a" ++ show i ++ ")" else " a" ++ show i)) (0::Int, "") params)
                tt -> throwError . ErrorString $ "Can only create a marshalling function for function-types. Found: " ++ show tt

typeDataToEnumMarshallCFunctionDefinition :: String -> Declaration.TypeConversionData -> CodeMonad()
typeDataToEnumMarshallCFunctionDefinition name tcd =
  case Declaration.convertedTypes tcd of
    Nothing -> throwError . ErrorString $ "Cannot compose a marshalling C function from Nothing"
    Just t -> typeData2EnumMarshallCFunctionDefinition
      where typeData2EnumMarshallCFunctionDefinition :: CodeMonad ()
            typeData2EnumMarshallCFunctionDefinition = do
              case t of
                (Declaration.FunctionType ret params) -> do
                  typeSignature <- typeData2CTypeSignature True t
                  addCCodeLine $ (typeSignature . Just) (name ++"_") ++ " {"
                  retTypeName <- typeData2CTypeSignature False ret
                  let (retL, retR) = if Declaration.isEnumType ret then ("from" ++ (pascal . retTypeName) Nothing ++ "(",")") else ("","")
                  ps <- intercalate ", " . snd <$> foldlM (\(i, s) p -> do
                      paramTypeName <- typeData2CTypeSignature False p
                      return (i+1,if Declaration.isEnumType p
                        then ("intTo" ++ (pascal . paramTypeName) Nothing ++ " (a" ++ show i ++ ")") : s
                        else ("a" ++ show i) : s)
                      ) (0::Int, []) params
                  addCCodeLine $ "  " ++ retL ++ name ++ "(" ++ ps ++ ")" ++ retR
                  addCCodeLine "}"
                tt -> throwError . ErrorString $ "Can only create marshalling C function from function-typs. Found: " ++ show tt

-- | Convert a given TypeData into a C type signature. 
-- 
-- This function returns a functions that converts a Maybe String into a c-type-signature. 
-- When given Nothing this function returns an anonymous type signature. Then given a "Just s" it 
-- returns a type declaration of the variable s. 
--
-- The flag indicates whether we want to apply the EnumType-to-CULong conversion
typeData2CTypeSignature :: Bool -> Declaration.TypeData -> CodeMonad (Maybe String -> String)
typeData2CTypeSignature flag t = case (flag, t) of

    (_, Declaration.SimpleType tt) -> case cLanguage tt of
      Left _ -> throwError . ErrorString $ "Cannot compose a C type signature from an unnamed type"
      Right q -> return $ \name -> q ++ maybe "" (" " ++) name

    (False, Declaration.EnumType tt) -> case cLanguage tt of
      Left _ -> throwError . ErrorString $ "Cannot compose a C type signature from an unnamed type"
      Right q -> return $ \name -> q ++ maybe "" (" " ++) name

    (True, Declaration.EnumType _) -> return $ \name -> "unsigned long" ++ maybe "" (" " ++ ) name

    (_, Declaration.CompType tt) -> case cLanguage tt of
      Left _ -> throwError . ErrorString $ "Cannot compose a C type signature from an unnamed type"
      Right q -> return $ \name -> q ++ maybe "" (" " ++) name

    (b, Declaration.ArrayType tt) -> do
      s <- typeData2CTypeSignature b tt
      return $ \name -> s Nothing ++ maybe "" (" " ++) name ++ "[]"

    (b, Declaration.PtrType tt) -> do
      s <- typeData2CTypeSignature b tt
      return $ \name -> s Nothing ++ maybe "" (" *" ++) name

    (b, Declaration.FunctionType ret params) -> do
      retS <- typeData2CTypeSignature b ret
      ps' <- mapM (typeData2CTypeSignature b) params
      let ps = intercalate ", " . snd $ foldl (\(i,as) s -> (i+1,(s Nothing ++ " a" ++ show i) : as )) (0::Int, []) ps'
      return $ \name -> retS name ++ "(" ++ ps ++ ")"

    (b, Declaration.TypeDefType tt tty) -> case cLanguage tt of
      Left _ -> throwError . ErrorString $ "Cannot compose a C type signature from an unnamed type"
      Right q -> if b && Declaration.isEnumType tty
        then return $ \name -> "unsigned long" ++ maybe "" (" " ++) name
        else return $ \name -> q ++ maybe "" (" " ++) name


typeData2HaskellTypeSignature :: Bool -> Declaration.TypeConversionData -> CodeMonad String
typeData2HaskellTypeSignature flag tcd = do
  case Declaration.convertedTypes tcd of
    Nothing -> throwError . ErrorString $ "Cannot compose a type signature from Nothing"
    Just t -> do
      let s = typeDataToHaskellSignature t
      case s of
        Left (Ast.Name nameId) ->
          throwError . ErrorString $
            "Cannot compose a Haskell type signature using unnamed types (" ++ show nameId ++ ") from: " ++ show (Declaration.convertedTypes tcd)
        Right w -> return w
 where
  typeDataToHaskellSignature :: Declaration.TypeData -> Either Ast.Name String
  typeDataToHaskellSignature tt =
    case (flag, tt) of
      (_, Declaration.SimpleType s) -> Declaration.haskellLanguage s
      (False, Declaration.EnumType s) -> Declaration.haskellLanguage s
      (True, Declaration.EnumType _) -> Right "CULong"
      (_, Declaration.CompType s) -> Declaration.haskellLanguage s
      (_, Declaration.PtrType t@(Declaration.FunctionType _ _)) ->
        typeDataToHaskellSignature t >>= \s -> return $ "FunPtr (" ++ s ++ ")"
      (_, Declaration.PtrType t@(Declaration.PtrType _)) ->
        typeDataToHaskellSignature t >>= \s -> return $ "Ptr (" ++ s ++ ")"
      (_, Declaration.PtrType t@(Declaration.ArrayType _)) ->
        typeDataToHaskellSignature t >>= \s -> return $ "Ptr (" ++ s ++ ")"
      (_, Declaration.PtrType t) ->
        typeDataToHaskellSignature t >>= \s -> return $ "Ptr " ++ s
      (_, Declaration.ArrayType t@(Declaration.FunctionType _ _)) ->
        typeDataToHaskellSignature t >>= \s -> return $ "FunPtr (" ++ s ++ ")"
      (_, Declaration.ArrayType t@(Declaration.PtrType _)) ->
        typeDataToHaskellSignature t >>= \s -> return $ "Ptr (" ++ s ++ ")"
      (_, Declaration.ArrayType t@(Declaration.ArrayType _)) ->
        typeDataToHaskellSignature t >>= \s -> return $ "Ptr (" ++ s ++ ")"
      (_, Declaration.ArrayType t) ->
        typeDataToHaskellSignature t >>= \s -> return $ "Ptr " ++ s
      (_, Declaration.FunctionType result params) -> do
        res <- typeDataToHaskellSignature result
        let res_s = case result of
              (Declaration.FunctionType _ _) -> "IO ( " ++ res ++ " )"
              (Declaration.PtrType _ ) -> "IO ( " ++ res ++ ")"
              (Declaration.ArrayType _) -> "IO ( " ++ res ++ ")"
              _ -> "IO " ++ res
        ps <-
          mapM
            ( \param -> do
                r <- typeDataToHaskellSignature param
                return $ case param of
                  (Declaration.FunctionType _ _) -> "( " ++ r ++ " )"
                  _ -> r
            )
            (reverse params)
        (return . intercalate " -> ") (ps ++ [res_s])
      (b, Declaration.TypeDefType s tty ) -> if b && Declaration.isEnumType tty
        then Right "CULong"
        else Declaration.haskellLanguage s

processTypeDef :: Ast.TypeDef -> CodeMonad ()
processTypeDef (Ast.TypeDef ident@(Ast.Ident name _ _) t _ _) = do
  if Declaration.isAnnonymousType t
    then case t of
      (Ast.DirectType (Ast.TyComp (Ast.CompTypeRef (Ast.AnonymousRef _) _ _)) _ _) -> return ()
      (Ast.DirectType (Ast.TyEnum (Ast.EnumTypeRef (Ast.AnonymousRef n) _)) _ _) -> do
        maybeTag <- getTags <&> Data.Map.lookup (Ast.AnonymousRef n)
        case maybeTag of
          Nothing -> throwError . ErrorString $ "Unknown AnonymousRef " ++ show n ++ " in processTypeDef."
          Just tag -> processTag (Ast.NamedRef ident) tag
      tt -> throwError . ErrorString $ "Unsupported type '" ++ show tt ++ "' in processTypeDef."
    else do
      tty <- Declaration.typeConversionData t
      q <- lift $ Declaration.runTypeConversion tty Declaration.cTypeToHaskellType
      s <- typeData2HaskellTypeSignature False q
      addHaskellCodeLine $ "newtype " ++ pascal name ++ " where " ++ pascal name ++ " :: { " ++ camel name ++ " :: " ++ s ++ " } -> " ++ pascal name

processVarDecl :: Ast.VarDecl -> CodeMonad ()
processVarDecl (Ast.VarDecl (Ast.VarName (Ast.Ident name _ _) _) _ ty) = do
  filePath <- getFileName
  let header = fromMaybe "" filePath
  tty <- Declaration.typeConversionData ty
  q <- lift $ Declaration.runTypeConversion tty Declaration.cTypeToHaskellType
  if Declaration.describesFunctionType q
    then if maybe False Declaration.containsEnumType (Declaration.convertedTypes q)
      then do
        s <- typeData2HaskellTypeSignature True q
        typeDataToEnumMarshallHaskellFunctionDefiniton (camel name) q
        typeDataToEnumMarshallCFunctionDefinition name q
        addHaskellCodeLine $ "foreign import capi safe \"" ++ header ++ " " ++ name ++ "\" " ++ camel name ++ "_ :: " ++ s
      else do
        s <- typeData2HaskellTypeSignature False q
        addHaskellCodeLine $ "foreign import capi safe \"" ++ header ++ " " ++ name ++ "\" " ++ camel name ++ " :: " ++ s
    else
      if Declaration.includesEnums q
        {- enums are to be excludedhere since every entry on each enum will be a declaration -}
        then return ()
        else do
          s <- typeData2HaskellTypeSignature False q
          addHaskellCodeLine $ "foreign import capi safe \"" ++ header ++ " value " ++ name ++ "\" " ++ camel name ++ " :: " ++ s
processVarDecl _ = throwError . ErrorString $ "Unhanded VarDecl in processVarDecl"

nodeStemsFromFilesToConsider :: (Ast.CNode a) => [String] -> a -> Bool
nodeStemsFromFilesToConsider as d = nodeInfoInFilesToConsider as $ Ast.nodeInfo d

nodeInfoInFilesToConsider :: [String] -> Ast.NodeInfo -> Bool
nodeInfoInFilesToConsider [] _ = False
nodeInfoInFilesToConsider _ (Ast.OnlyPos _ _) = False
nodeInfoInFilesToConsider (a : as) nodeInfo = (a == (Ast.posFile . Ast.posOfNode) nodeInfo) || nodeInfoInFilesToConsider as nodeInfo


