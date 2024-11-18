{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Number.Flint.TH.CodeMonad (
    CodeData(..)
  , TreeWalker(..)
  , CodeMonad
  , codeData
  , runCodeMonad
  , walkObjects
) where


import Data.Number.Flint.TH.Error ( ErrorString(..) )
import qualified Data.Number.Flint.TH.Declaration as Declaration
import Data.Number.Flint.TH.TreeWalker ( TreeWalker(..) )
import qualified Language.C.Analysis as Ast
import Control.Monad.State
    ( gets, MonadState(put, get), MonadTrans(lift), StateT(runStateT) )
import qualified Language.C as Ast
import qualified Data.Map
import qualified Language.C.Data.Ident as Ast
import Control.Monad.Error.Class ( MonadError(throwError) )
import Text.Casing ( camel, pascal )
import Control.Monad ( (>=>), foldM_ )
import Data.Maybe ( fromMaybe )
import Data.List ( intercalate )
import Data.Functor ( (<&>) )


data CodeData = CodeData {
    haskellCodeLinesOutput :: [String]
  , cCodeLinesOutput :: [String]
  , currentNodeInfo :: Maybe Ast.NodeInfo
  , globalDeclarations :: Ast.GlobalDecls
  , outputFiles :: [String]
  }

codeData :: [String] -> Ast.GlobalDecls -> CodeData
codeData output gDecls =
  CodeData{
    haskellCodeLinesOutput=[],
    cCodeLinesOutput=[],
    currentNodeInfo=Nothing,
    globalDeclarations=gDecls,
    outputFiles=output
  }


type CodeMonad = StateT CodeData (Either ErrorString)

runCodeMonad :: CodeData -> CodeMonad a -> Either ErrorString CodeData
runCodeMonad cd f =
  let a = runStateT f cd
  in case a of
    Left e -> Left e
    Right (_,b) -> Right b



instance (TreeWalker CodeMonad) where

  getNodeInfo :: CodeMonad (Maybe Ast.NodeInfo)
  getNodeInfo = do
    s <- get
    (return . currentNodeInfo) s


-- | enum values will be reported as declarations. We need to filter them out since they
-- are handled in another way. 
notIsEnumValue :: Ast.IdentDecl -> Bool
notIsEnumValue = notIsEnumValue' . Ast.getVarDecl
  where notIsEnumValue' (Ast.VarDecl _ _ ty) = notIsEnumValue'' ty
        notIsEnumValue'' (Ast.DirectType (Ast.TyEnum _) _ _) = False
        notIsEnumValue'' _ = True

walkObjects :: CodeMonad ()
walkObjects = do
  typeDefs <- getTypeDefs
  mapM_ (processTypeDef_ . snd) $ Data.Map.toList typeDefs
  objs <- getDeclarations
  mapM_ (processIdentDecl_ . snd) . filter (notIsEnumValue . snd) $ Data.Map.toList objs
      where processIdentDecl_ decl = setNodeInfo (Ast.nodeInfo decl) >> (processVarDecl . Ast.getVarDecl) decl
            processTypeDef_ typeDef = do
              setNodeInfo (Ast.nodeInfo typeDef)
              processTypeDef typeDef

getDeclarations :: CodeMonad (Data.Map.Map Ast.Ident Ast.IdentDecl)
getDeclarations = do
  cd <- get
  return . Data.Map.filter (nodeStemsFromOutputFiles $ outputFiles cd) $ Ast.gObjs (globalDeclarations cd)

getTags :: CodeMonad (Data.Map.Map Ast.SUERef Ast.TagDef)
getTags = do
  cd <- get
  return . Data.Map.filter (nodeStemsFromOutputFiles $ outputFiles cd) $ Ast.gTags (globalDeclarations cd)

getTypeDefs :: CodeMonad (Data.Map.Map Ast.Ident Ast.TypeDef)
getTypeDefs = do
  cd <- get
  return . Data.Map.filter (nodeStemsFromOutputFiles $ outputFiles cd) $ Ast.gTypeDefs (globalDeclarations cd)


setNodeInfo :: Ast.NodeInfo -> CodeMonad ()
setNodeInfo n = do
  s <- get
  put (s{currentNodeInfo = Just n})

getFileName :: CodeMonad (Maybe FilePath)
getFileName = gets (currentNodeInfo >=> Ast.fileOfNode)

addHaskellCodeLine :: String -> CodeMonad ()
addHaskellCodeLine str = do
  s <- get
  put s{haskellCodeLinesOutput= str : haskellCodeLinesOutput s}

addCCodeLine :: String -> CodeMonad ()
addCCodeLine str = do
  s <- get
  put s{cCodeLinesOutput = str : cCodeLinesOutput s}

processTag :: Ast.SUERef -> Ast.TagDef -> CodeMonad ()
processTag (Ast.NamedRef (Ast.Ident name _ _)) (Ast.EnumDef (Ast.EnumType _ enumerators _ _)) = do
  let cNames = map (\(Ast.Enumerator (Ast.Ident enumeratorName _ _ ) _ _ _) -> enumeratorName) enumerators
      haskellTypeConstructors = map (\(Ast.Enumerator (Ast.Ident enumeratorName _ _ ) _ _ _) -> pascal enumeratorName) enumerators
      haskellType = "data " ++ pascal name ++ " = " ++ intercalate " | " haskellTypeConstructors
  addHaskellCodeLine haskellType
  addHaskellCodeLine $ "instance Enum " ++ pascal name ++ " where"
  foldM_ (\i s -> do
          addHaskellCodeLine $ "  fromEnum " ++ s ++ " = " ++ show i
          return $ i + 1) (0::Int) haskellTypeConstructors
  foldM_ (\i s -> do
          addHaskellCodeLine $ "  toEnum " ++ show i ++ " = " ++ s
          return $ i + 1) (0::Int) haskellTypeConstructors
  addCCodeLine $ name ++ " intTo" ++ pascal name ++ "(int p) {"
  foldM_ (\i s -> do
          addCCodeLine $ "  if( p == " ++ show i ++ ") return " ++ s ++ ";"
          return $ i + 1 ) (0::Int) cNames
  addCCodeLine "}"
  addCCodeLine $ "int " ++ camel name ++ "ToInt(" ++ name ++ " p) {"
  foldM_ (\i s -> do
          addCCodeLine $ "  if( p == " ++ s ++ ") return " ++ show i ++ ";"
          return $ i + 1) (0::Int) cNames
  addCCodeLine "}"

processTag s t = throwError . ErrorString $ "Unknown combination of SUERef '" ++ show s ++ "'and TagDef '" ++ show t ++ "'."


typeDataToEnumMarshallHaskellFunctionDefiniton :: TypeData -> CodeMonad ()
typeDataToEnumMarshallHaskellFunctionDefiniton tt = 
  case tt of 
    (FunctionType ret params) -> do return ()
    t -> throwError . ErrorString $ "Can only create a marshalling function for function-types. Found: " ++ show t

typeData2HaskellTypeSignature :: Declaration.TypeConversionData -> CodeMonad String
typeData2HaskellTypeSignature tcd = do
  case Declaration.convertedTypes tcd of
    Nothing -> throwError . ErrorString $ "Cannot compose a type signature from Nothing"
    Just t -> do
      let s = typeDataToHaskellSignature t
      case s of
        Left (Ast.Name nameId) -> throwError . ErrorString $
          "Cannot compose a Haskell type signature using unnamed types (" ++ show nameId ++ ") from: " ++ show (Declaration.convertedTypes tcd)
        Right w -> return w
    where typeDataToHaskellSignature :: TypeData -> Either Ast.Name String 
          typeDataToHaskellSignature tt = 
            case tt of 
              (SimpleType s) -> haskellLanguage s
              (EnumType s) -> haskellLanguage s
              (CompType s) -> haskellLanguage s

              (PtrType t@(FunctionType _ _)) ->
                typeDataToHaskellSignature t >>= \s -> return $ "FunPtr (" ++ s ++ ")"
              (PtrType t@(PtrType _)) -> 
                typeDataToHaskellSignature t >>= \s -> return $ "Ptr (" ++ s ++ ")"
              (PtrType t@(ArrayType _)) ->
                typeDataToHaskellSignature t >>= \s -> return $ "Ptr (" ++ s ++ ")"
              (PtrType t) ->
                typeDataToHaskellSignature t >>= \s -> return $ "Ptr " ++ s

              (ArrayType t@(FunctionType _ _)) ->
                typeDataToHaskellSignature t >>= \s -> return $ "FunPtr (" ++ s ++ ")"
              (ArrayType t@(PtrType _)) -> 
                typeDataToHaskellSignature t >>= \s -> return $ "Ptr (" ++ s ++ ")"
              (ArrayType t@(ArrayType _)) ->
                typeDataToHaskellSignature t >>= \s -> return $ "Ptr (" ++ s ++ ")"
              (ArrayType t) ->
                typeDataToHaskellSignature t >>= \s -> return $ "Ptr " ++ s

              (FunctionType result params) -> do 
                res <- typeDataToHaskellSignature result
                let res_s = case result of 
                              (FunctionType _ _) -> "( " ++ res ++ " )"
                              _ -> res
                ps <- mapM (\param -> do 
                                        r <- typeDataToHaskellSignature param
                                        return $ case param of 
                                                    (FunctionType _ _) -> "( " ++ r ++ " )"
                                                    _ -> r
                                        ) (reverse params)
                (return . intercalate " -> ") (ps ++ [res_s])
                
              (TypeDefType s _) -> haskellLanguage s

processTypeDef :: Ast.TypeDef -> CodeMonad ()
processTypeDef (Ast.TypeDef ident@(Ast.Ident name _ _) t _ _) = do
  if Declaration.isAnnonymousType t
    then case t of
      (Ast.DirectType (Ast.TyComp (Ast.CompTypeRef (Ast.AnonymousRef name) _ _)) _ _) -> return ()
      (Ast.DirectType (Ast.TyEnum (Ast.EnumTypeRef (Ast.AnonymousRef name) _ )) _ _) -> do
        maybeTag <- getTags <&> Data.Map.lookup (Ast.AnonymousRef name)
        case maybeTag of
          Nothing -> throwError . ErrorString $ "Unknown AnonymousRef " ++ show name ++ " in processTypeDef."
          Just tag -> processTag (Ast.NamedRef ident) tag
      t -> throwError . ErrorString $ "Unsupported type '" ++ show t ++ "' in processTypeDef."
    else do
      tty <- Declaration.typeConversionData t
      q <- lift $ Declaration.runTypeConversion tty Declaration.cTypeToHaskellType
      s <- typeData2HaskellTypeSignature q
      addHaskellCodeLine $ "newtype " ++ pascal name ++ " = " ++ pascal name ++ "{ " ++ camel name ++ " :: " ++ s ++ " } -> " ++ pascal name

processVarDecl :: Ast.VarDecl -> CodeMonad ()
processVarDecl (Ast.VarDecl (Ast.VarName (Ast.Ident name _ _) _) _ ty) = do
  filePath <- getFileName
  let header = fromMaybe "" filePath
  tty <- Declaration.typeConversionData ty
  q <- lift $ Declaration.runTypeConversion tty Declaration.cTypeToHaskellType
  s <- typeData2HaskellTypeSignature q
  if Declaration.describesFunctionType q
    then addHaskellCodeLine $ "foreign import capi safe \"" ++ header ++ " " ++ name ++ "\" " ++ camel name ++ " :: " ++ s
    else if Declaration.includesEnums q
      {- enums are to be excludedhere since every entry on each enum will be a declaration -}
      then return ()
      else addHaskellCodeLine $ "foreign import capi safe \"" ++ header ++ " value " ++ name ++ "\" " ++ camel name ++ " :: " ++ s
processVarDecl _ = throwError . ErrorString $ "Unhanded VarDecl in processVarDecl"



nodeStemsFromOutputFiles :: (Ast.CNode a) => [String] -> a -> Bool
nodeStemsFromOutputFiles as d            = nodeInfoInOutputFiles as $ Ast.nodeInfo d

nodeInfoInOutputFiles :: [String] -> Ast.NodeInfo -> Bool
nodeInfoInOutputFiles [] _ = False
nodeInfoInOutputFiles _ (Ast.OnlyPos _ _) = False
nodeInfoInOutputFiles (a:as) nodeInfo = (a == (Ast.posFile . Ast.posOfNode) nodeInfo ) || nodeInfoInOutputFiles as nodeInfo

