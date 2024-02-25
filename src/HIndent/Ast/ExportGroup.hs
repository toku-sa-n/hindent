{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.ExportGroup
  ( ExportGroup
  , mkExportGroup
  ) where

import           Data.List.NonEmpty
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.Ast.Export
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type HsModule' = HsModule GHC.GhcPs
#else
type HsModule' = HsModule
#endif
data ExportGroup
  = ExportAll
  | NoExports
  | ExportList (NonEmpty Export)

mkExportGroup :: HsModule' -> ExportGroup
mkExportGroup HsModule {..} =
  case hsmodExports of
    Nothing       -> ExportAll
    Just (L _ []) -> NoExports
    Just _        -> ExportList (fromList [])
