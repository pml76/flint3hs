{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Data.Number.Flint.TH (

) where
import qualified Language.C.Analysis.AstAnalysis as AST

import qualified Language.C.Analysis as AST
import qualified Data.Map

import qualified Language.C.Analysis as Ast

import qualified Language.C.Data.Ident as Ast
import qualified Language.C.Data.Name as Ast
import Data.List (intercalate)
import qualified Language.C as CP
import qualified Language.C.System.GCC as CP
import Language.C.Analysis.TypeUtils (isFunctionType)
import System.OsPath
import Text.Casing

data ErrorString = ErrorString String
  deriving (Eq, Show)

data DeclData = FunctionDecl {
                    functionDeclName :: String
                  , functionTypeSignature :: String
                  , headerFile :: String
                } |
                VariableDecl {
                    variableDeclName :: String
                  , variableTypeSignature :: String
                  , headerFile :: String
                }

data TypeData = DataType { typeSignature :: String}
              | FunctionType { typeSignature :: String}

extractFilename :: CP.NodeInfo -> String
extractFilename = map toChar . unpack . snd . splitFileName . pack . map unsafeFromChar . CP.posFile . CP.posOfNode

cIntegralTypeNameToHaskell :: Ast.IntType -> Either ErrorString String
cIntegralTypeNameToHaskell Ast.TyBool    = Right "CBool"
cIntegralTypeNameToHaskell Ast.TyChar    = Right "CChar"
cIntegralTypeNameToHaskell Ast.TySChar   = Right "CSChar"
cIntegralTypeNameToHaskell Ast.TyUChar   = Right "CUChar"
cIntegralTypeNameToHaskell Ast.TyShort   = Right "CShort"
cIntegralTypeNameToHaskell Ast.TyUShort  = Right "CUShort"
cIntegralTypeNameToHaskell Ast.TyInt     = Right "CInt"
cIntegralTypeNameToHaskell Ast.TyUInt    = Right "CUInt"
cIntegralTypeNameToHaskell Ast.TyLong    = Right "CLong"
cIntegralTypeNameToHaskell Ast.TyULong   = Right "CULong"
cIntegralTypeNameToHaskell Ast.TyLLong   = Right "CLLong"
cIntegralTypeNameToHaskell Ast.TyULLong  = Right "CULLong"
cIntegralTypeNameToHaskell Ast.TyInt128  = Left $ ErrorString "128 bit integers are not supported"
cIntegralTypeNameToHaskell Ast.TyUInt128 = Left $ ErrorString "128 bit integers are not supported"

cFloatTypeNameToHaskell :: Ast.FloatType -> Either ErrorString String
cFloatTypeNameToHaskell Ast.TyFloat        = Right "CFloat"
cFloatTypeNameToHaskell Ast.TyDouble       = Right "CDouble"
cFloatTypeNameToHaskell Ast.TyLDouble      = Left $ ErrorString "Long Double is not supported"
cFloatTypeNameToHaskell (Ast.TyFloatN _ _) = Left $ ErrorString "floatN is not supported"

isSimpleTypeName :: Ast.TypeName -> Bool
isSimpleTypeName Ast.TyVoid = True
isSimpleTypeName (Ast.TyIntegral _) = True
isSimpleTypeName (Ast.TyFloating _ ) = True
isSimpleTypeName _ = False

cTypeNameToHaskell :: Ast.TypeName -> Either ErrorString String
cTypeNameToHaskell Ast.TyVoid         = Right "()"
cTypeNameToHaskell (Ast.TyIntegral t) = cIntegralTypeNameToHaskell t
cTypeNameToHaskell (Ast.TyFloating t) = cFloatTypeNameToHaskell t
cTypeNameToHaskell (Ast.TyComp (Ast.CompTypeRef (Ast.NamedRef (Ast.Ident name _ _)) _ _))  = Right name
cTypeNameToHaskell (Ast.TyComp (Ast.CompTypeRef (Ast.AnonymousRef (Ast.Name nameId)) _ _)) = Right $ "unnamedNo" ++ show nameId
cTypeNameToHaskell (Ast.TyEnum (Ast.EnumTypeRef (Ast.AnonymousRef (Ast.Name nameId)) _ ))  = Right $ "UnnamedEnumNo" ++ show nameId
cTypeNameToHaskell t                  = Left . ErrorString $ "unsupported type '" ++ show t ++ "' in cTypeNameToHaskell"

paramDeclFunc :: Ast.ParamDecl -> Either ErrorString String
paramDeclFunc (Ast.ParamDecl varDecl _) =
  case processDecl varDecl of
    Right (FunctionDecl _ s _ ) -> Right $  "(" ++ s ++ ")"
    Right (VariableDecl _ s _ ) -> Right s
    Left e -> Left e
paramDeclFunc (Ast.AbstractParamDecl varDecl nodeInfo) = paramDeclFunc (Ast.ParamDecl varDecl nodeInfo)

processDecl :: Ast.VarDecl -> Either ErrorString DeclData
processDecl (Ast.VarDecl (Ast.VarName (Ast.Ident name _ nodeInfo) _) _ ty) =
  case cTypeToHaskellType ty of
    Right s -> Right $
      case s of
        (DataType f) -> VariableDecl name f (extractFilename nodeInfo)
        (FunctionType f) -> FunctionDecl name f (extractFilename nodeInfo)
    Left e -> Left e
processDecl (Ast.VarDecl _ _ ty) =
  case cTypeToHaskellType ty of
    Right s -> Right $
      case s of
        (DataType f) -> VariableDecl "" f ""
        (FunctionType f) -> FunctionDecl "" f ""
    Left e -> Left e

isSimpleCType :: Ast.Type -> Bool
isSimpleCType (Ast.DirectType ty _ _) = isSimpleTypeName ty
isSimpleCType (Ast.PtrType ty _ _) = isSimpleCType ty
isSimpleCType (Ast.ArrayType ty _ _ _) = isSimpleCType ty
isSimpleCType _ = False

isPtrCType :: Ast.Type -> Bool
isPtrCType (Ast.PtrType {}) = True
isPtrCType (Ast.ArrayType{}) = True
isPtrCType _ = False

cTypeToHaskellType :: Ast.Type -> Either ErrorString TypeData
cTypeToHaskellType (Ast.DirectType tyName _ _) =
  case cTypeNameToHaskell tyName of
    Right s -> Right . DataType $ s
    Left e -> Left e
cTypeToHaskellType (Ast.PtrType ty _ _)        =
  case cTypeToHaskellType ty of
    Right (FunctionType s) -> Right . DataType $ "FunPtr " ++ "(" ++ s ++ ")"
    Right (DataType s) -> Right . DataType $ "Ptr " ++ if isPtrCType ty then "(" ++ s ++ ")" else s
    Left e -> Left e
cTypeToHaskellType (Ast.ArrayType ty _ _ _) =
  case cTypeToHaskellType ty of
    Right (FunctionType s) -> Right . DataType $ "FunPtr " ++ "(" ++ s ++ ")"
    Right (DataType s) -> Right . DataType $ "Ptr " ++ if isPtrCType ty then "(" ++ s ++ ")" else s
    Left e -> Left e
cTypeToHaskellType (Ast.FunctionType (Ast.FunType ty params _) _) =
  case cTypeToHaskellType ty of
    Right s ->
      let retType = "IO " ++ if isPtrCType ty then "(" ++ typeSignature s ++ ")" else typeSignature s
      in case mapM paramDeclFunc params of
        Right ls -> Right . FunctionType $ intercalate " -> " (ls ++ [retType])
        Left e -> Left e
    Left e -> Left e
cTypeToHaskellType (Ast.TypeDefType (Ast.TypeDefRef (Ast.Ident name _ _) ty _) _ _) =
  if isSimpleCType ty
    then cTypeToHaskellType ty
    else Right . DataType $ pascal name
cTypeToHaskellType t = Left . ErrorString $ "Unsupported type '" ++ "in cTypeToHaskellType"

isDeclarationInFileList :: [String] -> CP.NodeInfo -> Either String ()
isDeclarationInFileList _ (CP.OnlyPos _ _) = Left ""
isDeclarationInFileList [] nodeInfo = Left $ (CP.posFile . CP.posOfNode) nodeInfo
isDeclarationInFileList (a:as) nodeInfo =
  if  (CP.posFile . CP.posOfNode) nodeInfo == a
    then Right ()
    else isDeclarationInFileList as nodeInfo

processDeclaration :: Ast.IdentDecl -> Either ErrorString String
processDeclaration declaration =
  case declaration of
    Ast.Declaration (Ast.Decl varDecl nodeInfo) ->
      case processDecl varDecl of
        Right (FunctionDecl name s header) -> Right $ "foreign import capi safe \"" ++ header ++ " " ++ name ++ "\" " ++ camel name ++ " :: " ++ s
        Right (VariableDecl name s header) -> Right $ "foreign import capi safe \"" ++ header ++ " value " ++ name ++ "\" " ++ camel name ++ " :: " ++ s
        Left e -> Left e
    Ast.FunctionDef (Ast.FunDef varDecl _ nodeInfo) ->
      processDeclaration $ Ast.Declaration (Ast.Decl varDecl nodeInfo)
    Ast.EnumeratorDef (Ast.Enumerator (Ast.Ident name _ _) _ enumType nodeInfo) ->
      Right $ "Enumerator: " ++  name
    t -> Left . ErrorString $ "unsupported declaration '" ++ show t ++ "' in processDeclaration"

filterDeclarations :: [String] -> Data.Map.Map Ast.Ident Ast.IdentDecl -> Data.Map.Map Ast.Ident Ast.IdentDecl
filterDeclarations outputFiles = Data.Map.filter (isDeclarationInOutputFiles outputFiles)
  where isDeclarationInOutputFiles :: [String] -> Ast.IdentDecl -> Bool
        isDeclarationInOutputFiles as (Ast.Declaration (Ast.Decl _ nodeInfo))             = nodeInfoInOutputFiles as nodeInfo
        isDeclarationInOutputFiles as (Ast.ObjectDef (Ast.ObjDef _ _ nodeInfo))           = nodeInfoInOutputFiles as nodeInfo
        isDeclarationInOutputFiles as (Ast.FunctionDef (Ast.FunDef _ _ nodeInfo))         = nodeInfoInOutputFiles as nodeInfo
        isDeclarationInOutputFiles as (Ast.EnumeratorDef (Ast.Enumerator _ _ _ nodeInfo)) = nodeInfoInOutputFiles as nodeInfo

nodeInfoInOutputFiles :: [String] -> CP.NodeInfo -> Bool
nodeInfoInOutputFiles [] _ = False
nodeInfoInOutputFiles _ (CP.OnlyPos _ _) = False
nodeInfoInOutputFiles (a:as) nodeInfo = (a == (CP.posFile . CP.posOfNode) nodeInfo ) || nodeInfoInOutputFiles as nodeInfo

filterTypeDefs :: [String] -> Data.Map.Map Ast.Ident Ast.TypeDef -> Data.Map.Map Ast.Ident Ast.TypeDef
filterTypeDefs outputFiles = Data.Map.filter (isTypeDefInOutputFile outputFiles)
  where isTypeDefInOutputFile :: [String] -> Ast.TypeDef -> Bool 
        isTypeDefInOutputFile as (Ast.TypeDef _ _ _ nodeInfo) = nodeInfoInOutputFiles as nodeInfo

convertDecarations :: Data.Map.Map Ast.Ident Ast.IdentDecl -> IO ()
convertDecarations m = do
  print $ Data.Map.size m
  let q = Data.Map.map processDeclaration m
  print $ map snd $ Data.Map.toList q

convertTypeDefs :: Data.Map.Map Ast.Ident Ast.TypeDef -> IO ()
convertTypeDefs m = do 
  print $ Data.Map.size m 
  let q = Data.Map.map processTypeDef m 
  print $ map snd $ Data.Map.toList q


processTypeDef :: Ast.TypeDef -> Either ErrorString String
processTypeDef (Ast.TypeDef (Ast.Ident name _ _ ) ty _ _) = 
  case cTypeToHaskellType ty of 
    Right (DataType s) -> Right $ name ++ ": " ++ s 
    Right (FunctionType s) -> Right $ name ++ ": " ++ s 
    Left e -> Left e

parseCFile :: FilePath -> [String] -> IO ()
parseCFile filePath outputFiles = do
  result <- CP.parseCFile (CP.newGCC "gcc") Nothing ["-I/mingw64/include", "-I/mingw64/include/flint"] filePath
  case result of
       Right r ->
        let globalDecls = AST.runTrav () (AST.analyseAST r)
        in case globalDecls of
          -- Right q -> convertDecarations $ filterDeclarations outputFiles $ AST.gObjs (fst q)
          Right q -> convertTypeDefs $ filterTypeDefs outputFiles $ Ast.gTypeDefs (fst q)
          _ -> error "error1"
       Left e -> error "error2"