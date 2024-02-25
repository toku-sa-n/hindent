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
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

data ModuleWarningOrDeprecated = ModuleWarningOrDeprecated
  { kind :: Kind
  , reason :: String
  }

instance CommentExtraction ModuleWarningOrDeprecated where
  nodeComments _ = NodeComments [] [] []

instance Pretty ModuleWarningOrDeprecated where
  pretty' ModuleWarningOrDeprecated {..} = do
    string "{-# "
    pretty kind
    space
    string reason
    string " #-}"

data Kind
  = Warning
  | Deprecated

instance CommentExtraction Kind where
  nodeComments _ = NodeComments [] [] []

instance Pretty Kind where
  pretty' Warning = string "WARNING"
  pretty' Deprecated = string "DEPRECATED"
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkModuleWarningOrDeprecated ::
     HsModule GhcPs -> Maybe (WithComments ModuleWarningOrDeprecated)
mkModuleWarningOrDeprecated HsModule {hsmodExt = XModulePs {..}} =
  case hsmodDeprecMessage of
    Nothing -> Nothing
    Just (L _ (WarningTxt _ _ [reason])) ->
      Just
        (mkWithCommentsWithEmptyComments
           (ModuleWarningOrDeprecated
              {kind = Warning, reason = showOutputable reason}))
    Just (L _ (WarningTxt _ _ _)) -> error "implement me"
    Just (L _ (DeprecatedTxt _ [reason])) ->
      Just
        (mkWithCommentsWithEmptyComments
           (ModuleWarningOrDeprecated
              {kind = Deprecated, reason = showOutputable reason}))
    Just (L _ (DeprecatedTxt _ _)) -> error "implement me"
#else
mkModuleWarningOrDeprecated ::
     HsModule -> Maybe (WithComments ModuleWarningOrDeprecated)
mkModuleWarningOrDeprecated HsModule {..} =
  case hsmodDeprecMessage of
    Nothing -> Nothing
    Just (L _ (WarningTxt _ [reason])) ->
      Just
        (mkWithCommentsWithEmptyComments
           (ModuleWarningOrDeprecated
              {kind = Warning, reason = showOutputable reason}))
    Just (L _ (WarningTxt _ _)) -> error "implement me"
    Just (L _ (DeprecatedTxt _ [reason])) ->
      Just
        (mkWithCommentsWithEmptyComments
           (ModuleWarningOrDeprecated
              {kind = Deprecated, reason = showOutputable reason}))
    Just (L _ (DeprecatedTxt _ _)) -> error "implement me"
#endif
