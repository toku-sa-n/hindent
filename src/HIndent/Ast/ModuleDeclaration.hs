-- | Module declaration AST.
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.ModuleDeclaration
  ( ModuleDeclaration
  , mkModuleDeclaration
  ) where

import qualified GHC.Hs as GHC
import HIndent.Ast.ExportGroup
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

data ModuleDeclaration = ModuleDeclaration
  { name :: String
  , exports :: ExportGroup
  }

instance CommentExtraction ModuleDeclaration where
  nodeComments ModuleDeclaration {} = NodeComments [] [] []

instance Pretty ModuleDeclaration where
  pretty' ModuleDeclaration {..} = do
    string "module "
    string name
    newline
    pretty' exports
    string "where"
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
          {name = showOutputable name, exports = mkExportGroup m}
