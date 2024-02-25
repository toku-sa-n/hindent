-- | Module declaration AST.
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.Declaration
  ( ModuleDeclaration
  , mkModuleDeclaration
  ) where

import Control.Monad
import HIndent.Applicative
import HIndent.Ast.ExportGroup
import HIndent.Ast.Module.Name
import HIndent.Ast.Module.WarningOrDeprecated
import HIndent.Ast.WithComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
import GHC.Hs hiding (ModuleName, mkModuleName)
#else
import GHC.Hs
#endif
data ModuleDeclaration = ModuleDeclaration
  { name :: WithComments ModuleName
  , warning :: Maybe (WithComments ModuleWarningOrDeprecated)
  , exports :: ExportGroup
  }

instance CommentExtraction ModuleDeclaration where
  nodeComments ModuleDeclaration {} = NodeComments [] [] []

instance Pretty ModuleDeclaration where
  pretty' ModuleDeclaration {..} = do
    pretty name
    whenJust warning $ \w -> do
      space
      pretty w
    when (hasExportList exports) $ do
      newline
      indentedBlock $ pretty exports
    string " where"
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkModuleDeclaration :: HsModule GhcPs -> Maybe ModuleDeclaration
#else
mkModuleDeclaration :: HsModule -> Maybe ModuleDeclaration
#endif
mkModuleDeclaration m =
  case mkModuleName m of
    Nothing -> Nothing
    Just name ->
      Just
        ModuleDeclaration
          { name
          , warning = mkModuleWarningOrDeprecated m
          , exports = mkExportGroup m
          }
