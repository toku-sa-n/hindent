-- | Module declaration AST.
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.ModuleDeclaration
  ( ModuleDeclaration
  , mkModuleDeclaration
  ) where

import qualified GHC.Hs                                as GHC
import           HIndent.Ast.WithComments
import           HIndent.Pretty.Combinators.Outputable
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type HsModule' = GHC.HsModule GHC.GhcPs
#else
type HsModule' = GHC.HsModule
#endif
data ModuleDeclaration = ModuleDeclaration
  { name    :: WithComments String
  , module' :: HsModule'
  }

instance CommentExtraction ModuleDeclaration where
  nodeComments ModuleDeclaration {} = NodeComments [] [] []
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkModuleDeclaration :: GHC.HsModule GHC.GhcPs -> ModuleDeclaration
#else
mkModuleDeclaration :: GHC.HsModule -> Maybe ModuleDeclaration
#endif
mkModuleDeclaration m@GHC.HsModule {..} =
  case hsmodName of
    Nothing -> Nothing
    Just name ->
      Just
        ModuleDeclaration
          { name =
              WithComments
                {comments = NodeComments [] [] [], node = showOutputable name}
          , module' = m
          }
