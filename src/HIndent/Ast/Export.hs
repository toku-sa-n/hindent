-- | AST for an element of an export list.
{-# LANGUAGE CPP #-}

module HIndent.Ast.Export
  ( Export
  , mkExport
  ) where

import GHC.Hs

newtype Export =
  Export (LIE GhcPs)

mkExport :: LIE GhcPs -> Export
mkExport = Export
