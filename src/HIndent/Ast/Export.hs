-- | AST for an element of an export list.
{-# LANGUAGE CPP #-}

module HIndent.Ast.Export
  ( Export
  , mkExport
  ) where

import GHC.Hs
import HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype Export =
  Export (IE GhcPs)

instance CommentExtraction Export where
  nodeComments (Export x) = nodeComments x

instance Pretty Export where
  pretty' (Export x) = pretty' x

mkExport :: IE GhcPs -> Export
mkExport = Export
