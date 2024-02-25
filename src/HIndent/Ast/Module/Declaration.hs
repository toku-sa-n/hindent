-- | Module declaration AST.
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.Declaration
  ( ModuleDeclaration
  , mkModuleDeclaration
  ) where

import Control.Monad
import qualified GHC.Hs as GHC
import HIndent.Ast.ExportGroup
import HIndent.Ast.Module.WarningOrDeprecated
import HIndent.Ast.WithComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

data ModuleDeclaration = ModuleDeclaration
  { name :: String
  , warning :: Maybe (WithComments ModuleWarningOrDeprecated)
  , exports :: ExportGroup
  }

instance CommentExtraction ModuleDeclaration where
  nodeComments ModuleDeclaration {} = NodeComments [] [] []

instance Pretty ModuleDeclaration where
  pretty' ModuleDeclaration {..} = do
    string "module "
    string name
    when (hasExportList exports) $ do
      newline
      indentedBlock $ pretty exports
    string " where"
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkModuleDeclaration :: GHC.HsModule GHC.GhcPs -> Maybe ModuleDeclaration
#else
mkModuleDeclaration :: GHC.HsModule -> Maybe ModuleDeclaration
#endif
mkModuleDeclaration m@GHC.HsModule {..} =
  case hsmodName of
    Nothing -> Nothing
    Just name ->
      Just
        ModuleDeclaration
          { name = showOutputable name
          , warning = mkModuleWarningOrDeprecated m
          , exports = mkExportGroup m
          }
