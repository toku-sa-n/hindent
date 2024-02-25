{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import
  ( Import
  , mkImport
  ) where

import GHC.Hs

newtype Import =
  Import (LImportDecl GhcPs)
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkImport :: HsModule GhcPs -> Import
#else
mkImport :: HsModule -> [Import]
#endif
mkImport HsModule {..} = fmap Import hsmodImports
