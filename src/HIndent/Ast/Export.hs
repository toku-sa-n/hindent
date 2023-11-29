-- | AST for an element of an export list.
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Export
  ( Export
  , mkExport
  ) where

import           GHC.Hs
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type HsModule' = HsModule GhcPs
#else
type HsModule' = HsModule
#endif
newtype Export =
  Export HsModule'

mkExport :: HsModule' -> Export
mkExport = Export
