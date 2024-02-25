{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.WarningOrDeprecated
  ( ModuleWarningOrDeprecated
  , mkModuleWarningOrDeprecated
  ) where

import GHC.Hs hiding (Warning)
import GHC.Types.SrcLoc
import GHC.Unit.Module.Warnings
import HIndent.Ast.WithComments

data ModuleWarningOrDeprecated
  = Warning String
  | Deprecated String
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkModuleWarningOrDeprecated ::
     HsModule GhcPs -> Maybe (WithComments ModuleWarningOrDeprecated)
#else
mkModuleWarningOrDeprecated ::
     HsModule -> Maybe (WithComments ModuleWarningOrDeprecated)
#endif
mkModuleWarningOrDeprecated HsModule {..} =
  case hsmodDeprecMessage of
    Nothing -> Nothing
    Just (L _ (WarningTxt _ _)) ->
      Just (mkWithCommentsWithEmptyComments (Warning "eee"))
    Just (L _ (DeprecatedTxt _ _)) ->
      Just (mkWithCommentsWithEmptyComments (Deprecated "eee"))
