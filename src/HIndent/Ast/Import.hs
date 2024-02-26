-- TODO: Split this file into multiple files
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
{-# LANGUAGE ViewPatterns #-}
#endif
module HIndent.Ast.Import
  ( Import
  , mkImport
  , sortImportsAndEntriesByName
  ) where

import Control.Monad
import Data.Function
import Data.List
import qualified GHC.Hs as GHC
import HIndent.Applicative
import HIndent.Ast.Import.Entry
import HIndent.Ast.WithComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
#if !MIN_VERSION_ghc_lib_parser(9, 6, 1)
import qualified GHC.Unit.Types as GHC
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
import qualified GHC.Types.PkgQual as GHC
#endif
data Import = Import
  { moduleName :: WithComments String
  , isSourceImport :: Bool
  , isSafeImport :: Bool
  , qualification :: Qualification
  , packageName :: Maybe String
  , list :: Maybe (WithComments ImportEntries)
  }

instance CommentExtraction Import where
  nodeComments Import {} = NodeComments [] [] []

instance Pretty Import where
  pretty' Import {..} = do
    string "import "
    when isSourceImport $ string "{-# SOURCE #-} "
    when isSafeImport $ string "safe "
    unless (qualification == NotQualified) $ string "qualified "
    whenJust packageName $ \name -> string name >> space
    printCommentsAnd moduleName string
    case qualification of
      QualifiedAs name -> string " as " >> printCommentsAnd name string
      _ -> pure ()
    whenJust list pretty

data Qualification
  = NotQualified
  | FullyQualified
  | QualifiedAs (WithComments String)
  deriving (Eq)

mkImport :: GHC.ImportDecl GHC.GhcPs -> Import
mkImport import'@GHC.ImportDecl {..} =
  Import
    { moduleName = showOutputable <$> mkWithCommentsWithGenLocated ideclName
    , isSourceImport = ideclSource == GHC.IsBoot
    , isSafeImport = ideclSafe
    , packageName = getPackageName import'
    , qualification
    , list
    }
  where
    qualification =
      case (ideclQualified, ideclAs) of
        (GHC.NotQualified, _) -> NotQualified
        (_, Nothing) -> FullyQualified
        (_, Just name) ->
          QualifiedAs $ showOutputable <$> mkWithCommentsWithGenLocated name
    list = mkImportEntries import'

getPackageName :: GHC.ImportDecl GHC.GhcPs -> Maybe String
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
getPackageName (GHC.ideclPkgQual -> GHC.RawPkgQual name) =
  Just $ showOutputable name
getPackageName _ = Nothing
#else
getPackageName = fmap showOutputable . GHC.ideclPkgQual
#endif
-- | This function sorts import declarations and explicit imports in them
-- by their names.
sortImportsAndEntriesByName :: [WithComments Import] -> [WithComments Import]
sortImportsAndEntriesByName = fmap sortImportEntries . sortByModuleName

-- | This function sorts explicit imports in the given import declaration
-- by their names.
sortImportEntries :: WithComments Import -> WithComments Import
sortImportEntries = fmap f
  where
    f Import {..} = Import {list = sorted, ..}
      where
        sorted = fmap (fmap sortEntriesAndVariants) list

-- | This function sorts import declarations by their module names.
sortByModuleName :: [WithComments Import] -> [WithComments Import]
sortByModuleName = sortBy (compare `on` getNode . moduleName . getNode)
