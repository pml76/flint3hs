{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Number.Flint.TH (
  parseCFile
, extractFilename
, handleConversion) where
import qualified Language.C.Analysis.AstAnalysis as AST

import qualified Language.C.Analysis as AST
import qualified Data.Map
import qualified Language.C as CP
import qualified Language.C.System.GCC as CP
import System.OsPath
    ( splitFileName, pack, unpack, toChar, unsafeFromChar )

import qualified Data.Number.Flint.TH.CodeMonad as CodeMonad 
import Data.Number.Flint.TH.Error ( ErrorString(..) ) 

extractFilename :: CP.NodeInfo -> String
extractFilename = map toChar . unpack . snd . splitFileName . pack . map unsafeFromChar . CP.posFile . CP.posOfNode

handleConversion ::  (b -> Either ErrorString String) -> Data.Map.Map a b -> IO ()
handleConversion f m = do
  print $ Data.Map.size m
  let q = Data.Map.map f m
  mapM_ output q
  putStrLn "";
    where output :: Either ErrorString String -> IO ()
          output (Left e) = print e
          output (Right s) = putStrLn s


outputResults :: [String] -> IO ()
outputResults [] = return ()
outputResults (a:as) = do
  putStrLn a 
  outputResults as

parseCFile :: FilePath -> [String] -> IO ()
parseCFile filePath files = do
  result <- CP.parseCFile (CP.newGCC "gcc") Nothing ["-I/mingw64/include", "-I/mingw64/include/flint"] filePath
  case result of
       Right r ->
        let globalDecls = AST.runTrav () (AST.analyseAST r)
        in case globalDecls of
          Right q -> do
            let cd = (CodeMonad.codeData files "t.hs" "t.h". fst) q
                res = CodeMonad.runCodeMonad cd CodeMonad.walkObjects
            case res of
              Left e -> print e
              Right c -> do 
                outputResults . reverse $ CodeMonad.haskellCodeLinesOutput c
                outputResults . reverse $ CodeMonad.cCodeLinesOutput c 
            -- handleConversion processDeclaration . Data.Map.filter (isDeclarationInfiles files) $ AST.gObjs (fst q)
            -- handleConversion processTypeDef . Data.Map.filter (isTypeDefInOutputFile files) $ Ast.gTypeDefs (fst q)
            -- handleConversion processTag . Data.Map.filter (isTagInOutputFile files ) $ Ast.gTags (fst q)
          _ -> error "error1"
       Left e -> error $ "error2" ++ show e