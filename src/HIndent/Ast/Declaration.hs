{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration
  ( DeclarationCollection
  , mkDeclarationCollection
  ) where

import GHC.Hs

newtype DeclarationCollection =
  DeclarationCollection [LHsDecl GhcPs]
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkDeclarationCollection :: HsModule GhcPs -> DeclarationCollection
#else
mkDeclarationCollection :: HsModule -> DeclarationCollection
#endif
mkDeclarationCollection HsModule {..} = DeclarationCollection hsmodDecls
