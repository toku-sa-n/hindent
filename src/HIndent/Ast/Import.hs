{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
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
          True -> pure $ extractImportsSorted' $ fmap (\(Import x) -> x) imports
          False -> pure $ extractImports' $ fmap (\(Import x) -> x) imports

newtype Import =
  Import (LImportDecl GhcPs)
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkImportCollection :: HsModule GhcPs -> ImportCollection
#else
mkImportCollection :: HsModule -> ImportCollection
#endif
mkImportCollection HsModule {..} = ImportCollection $ fmap Import hsmodImports

hasImports :: ImportCollection -> Bool
hasImports (ImportCollection imports) = not $ null imports
