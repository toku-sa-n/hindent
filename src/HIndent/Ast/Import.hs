{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import
  ( ImportCollection
  , mkImportCollection
  ) where

import GHC.Hs

newtype ImportCollection =
  ImportCollection [Import]

newtype Import =
  Import (LImportDecl GhcPs)
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkImportCollection :: HsModule GhcPs -> ImportCollection
#else
mkImportCollection :: HsModule -> ImportCollection
#endif
mkImportCollection HsModule {..} = ImportCollection $ fmap Import hsmodImports
