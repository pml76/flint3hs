module Data.Number.Flint.TH.TreeWalker (
    TreeWalker(..)
) where

    
import qualified Language.C as Ast


class (Monad m) => TreeWalker m where
  getNodeInfo :: m (Maybe Ast.NodeInfo)


