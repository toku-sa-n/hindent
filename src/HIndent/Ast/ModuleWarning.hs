{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.ModuleWarning
  ( ModuleWarning
  , mkModuleWarning
  ) where

import GHC.Hs hiding (Warning)
import GHC.Types.SrcLoc
import GHC.Unit.Module.Warnings
import HIndent.Ast.WithComments

data ModuleWarning
  = Warning String
  | Deprecated String
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkModuleWarning :: HsModule GhcPs -> Maybe (WithComments ModuleWarning)
#else
mkModuleWarning :: HsModule -> Maybe (WithComments ModuleWarning)
#endif
mkModuleWarning HsModule {..} =
  case hsmodDeprecMessage of
    Nothing -> Nothing
    Just (L _ (WarningTxt _ _)) ->
      Just (mkWithCommentsWithEmptyComments (Warning "eee"))
    Just (L _ (DeprecatedTxt _ _)) ->
      Just (mkWithCommentsWithEmptyComments (Deprecated "eee"))
