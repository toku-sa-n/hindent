{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.ExportGroup
  ( ExportGroup
  , mkExportGroup
  ) where

import Data.List.NonEmpty
import GHC.Hs
import GHC.Types.SrcLoc
import HIndent.Ast.Export
import HIndent.Ast.WithComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type HsModule' = HsModule GhcPs
#else
type HsModule' = HsModule
#endif
data ExportGroup
  = ExportAll
  | NoExports
  | ExportList (NonEmpty (WithComments Export))

mkExportGroup :: HsModule' -> ExportGroup
mkExportGroup HsModule {..} =
  case hsmodExports of
    Nothing -> ExportAll
    Just (L _ exports) ->
      case nonEmpty exports of
        Nothing -> NoExports
        Just exports' ->
          ExportList
            $ fmap (fmap mkExport . mkWithCommentsWithGenLocated) exports'
