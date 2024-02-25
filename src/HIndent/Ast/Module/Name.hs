{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.Name
  ( ModuleName
  , mkModuleName
  ) where
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
import GHC.Hs hiding (ModuleName, mkModuleName)
#else
import GHC.Hs
#endif
import HIndent.Ast.WithComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

newtype ModuleName =
  ModuleName String
  deriving (Eq, Show)

instance CommentExtraction ModuleName where
  nodeComments (ModuleName _) = NodeComments [] [] []

instance Pretty ModuleName where
  pretty' (ModuleName name) = string "module " >> string name
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkModuleName :: HsModule GhcPs -> Maybe (WithComments ModuleName)
#else
mkModuleName :: HsModule -> Maybe (WithComments ModuleName)
#endif
mkModuleName HsModule {..} =
  case hsmodName of
    Nothing -> Nothing
    Just name ->
      Just
        (ModuleName <$> mkWithCommentsWithGenLocated (fmap showOutputable name))
