{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import
  ( ImportCollection
  , mkImportCollection
  , hasImports
  ) where

import Control.Monad.RWS
import GHC.Hs
import HIndent.Config
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.Import
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
import HIndent.Printer

newtype ImportCollection =
  ImportCollection [Import]

instance CommentExtraction ImportCollection where
  nodeComments (ImportCollection _) = NodeComments [] [] []

instance Pretty ImportCollection where
  pretty' (ImportCollection imports) = prettyImports
    where
      prettyImports = importDecls >>= blanklined . fmap outputImportGroup
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True -> pure $ extractImportsSorted' $ fmap import' imports
          False -> pure $ extractImports' $ fmap import' imports

data Import = Import
  { isSafeImport :: Bool
  , import' :: LImportDecl GhcPs
  }
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkImportCollection :: HsModule GhcPs -> ImportCollection
#else
mkImportCollection :: HsModule -> ImportCollection
#endif
mkImportCollection HsModule {..} = ImportCollection $ fmap mkImport hsmodImports

mkImport :: LImportDecl GhcPs -> Import
mkImport import' = Import {isSafeImport = True, import'}

hasImports :: ImportCollection -> Bool
hasImports (ImportCollection imports) = not $ null imports
