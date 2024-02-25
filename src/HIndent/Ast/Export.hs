-- | AST for an element of an export list.
{-# LANGUAGE CPP #-}

module HIndent.Ast.Export
  ( Export
  , mkExport
  ) where

import GHC.Hs

newtype Export =
  Export (IE GhcPs)

mkExport :: IE GhcPs -> Export
mkExport = Export
