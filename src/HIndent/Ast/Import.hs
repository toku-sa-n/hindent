-- TODO: Split this file into multiple files
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
{-# LANGUAGE ViewPatterns #-}
#endif
module HIndent.Ast.Import
  ( ImportCollection
  , mkImportCollection
  , hasImports
  ) where

import Control.Monad.RWS
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import qualified GHC.Hs as GHC
import GHC.Stack
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.WithComments
import HIndent.Config
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
import HIndent.Printer
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
import qualified GHC.Types.PkgQual as GHC
#else
import qualified GHC.Unit.Types as GHC
#endif
newtype ImportCollection =
  ImportCollection [[WithComments Import]] -- Imports are not sorted by their names.

instance CommentExtraction ImportCollection where
  nodeComments (ImportCollection _) = NodeComments [] [] []

instance Pretty ImportCollection where
  pretty' (ImportCollection imports) = prettyImports
    where
      prettyImports = importDecls >>= blanklined . fmap outputImportGroup
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True -> pure $ fmap sortImportsByName imports
          False -> pure imports

data Import = Import
  { moduleName :: WithComments String
  , isSourceImport :: Bool
  , isSafeImport :: Bool
  , qualification :: Qualification
  , packageName :: Maybe String
  , list :: ImportEntries
  , import' :: GHC.ImportDecl GHC.GhcPs
  }

instance CommentExtraction Import where
  nodeComments Import {} = NodeComments [] [] []

instance Pretty Import where
  pretty' Import {..} = pretty import'

data Qualification
  = NotQualified
  | FullyQualified
  | QualifiedAs (WithComments String)

data ImportEntries
  = ExplicitImports (WithComments [GHC.LIE GHC.GhcPs])
  | Hiding (WithComments [GHC.LIE GHC.GhcPs])
  | NoImportEntries
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkImportCollection :: GHC.HsModule GHC.GhcPs -> ImportCollection
#else
mkImportCollection :: GHC.HsModule -> ImportCollection
#endif
mkImportCollection GHC.HsModule {..} =
  ImportCollection
    $ fmap (fmap mkImport . mkWithCommentsWithGenLocated)
        <$> extractImports hsmodImports

mkImport :: GHC.ImportDecl GHC.GhcPs -> Import
mkImport import'@GHC.ImportDecl {..} =
  Import
    { moduleName = showOutputable <$> mkWithCommentsWithGenLocated ideclName
    , isSourceImport = ideclSource == GHC.IsBoot
    , isSafeImport = ideclSafe
    , packageName = getPackageName import'
    , qualification
    , list
    , import'
    }
  where
    qualification =
      case (ideclQualified, ideclAs) of
        (GHC.NotQualified, _) -> NotQualified
        (_, Nothing) -> FullyQualified
        (_, Just name) ->
          QualifiedAs $ showOutputable <$> mkWithCommentsWithGenLocated name
    list = getImportList import'

hasImports :: ImportCollection -> Bool
hasImports (ImportCollection imports) = not $ null imports

getPackageName :: GHC.ImportDecl GHC.GhcPs -> Maybe String
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
getPackageName (GHC.ideclPkgQual -> GHC.RawPkgQual name) =
  Just $ showOutputable name
getPackageName _ = Nothing
#else
getPackageName = fmap showOutputable . GHC.ideclPkgQual
#endif
getImportList :: GHC.ImportDecl GHC.GhcPs -> ImportEntries
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
getImportList GHC.ImportDecl {..} =
  case ideclImportList of
    Just (GHC.Exactly, imports) ->
      ExplicitImports $ mkWithCommentsWithGenLocated imports
    Just (GHC.EverythingBut, imports) ->
      Hiding $ mkWithCommentsWithGenLocated imports
    Nothing -> NoImportEntries
#else
getImportList GHC.ImportDecl {..} =
  case ideclHiding of
    Just (False, imports) ->
      ExplicitImports $ mkWithCommentsWithGenLocated imports
    Just (True, imports) -> Hiding $ mkWithCommentsWithGenLocated imports
    Nothing -> NoImportEntries
#endif
extractImports :: [GHC.LImportDecl GHC.GhcPs] -> [[GHC.LImportDecl GHC.GhcPs]]
extractImports = groupImports . sortImportsByLocation

-- | Combines adjacent import declarations into a single list.
groupImports :: [GHC.LImportDecl GHC.GhcPs] -> [[GHC.LImportDecl GHC.GhcPs]]
groupImports = groupImports' []
  where
    groupImports' ::
         [[GHC.LImportDecl GHC.GhcPs]]
      -> [GHC.LImportDecl GHC.GhcPs]
      -> [[GHC.LImportDecl GHC.GhcPs]]
    groupImports' xs [] = xs
    groupImports' [] (x:xs) = groupImports' [[x]] xs
    groupImports' [[]] (x:xs) = groupImports' [[x]] xs
    groupImports' ([]:x:xs) (y:ys) = groupImports' ([y] : x : xs) ys
    groupImports' ((z:zs):xs) (y:ys)
      | z `isAdjacentTo` y = groupImports' ((y : z : zs) : xs) ys
      | otherwise = groupImports' ([y] : (z : zs) : xs) ys
    a `isAdjacentTo` b =
      GHC.srcSpanEndLine (sp a) + 1 == GHC.srcSpanStartLine (sp b)
        || GHC.srcSpanEndLine (sp b) + 1 == GHC.srcSpanStartLine (sp a)
    sp x =
      case GHC.locA $ GHC.getLoc x of
        GHC.RealSrcSpan x' _ -> x'
        _ -> error "Src span unavailable."

-- | The letter type of a 'Char'.
--
-- The order of constructors is important. HIndent sorts explicit imports
-- from ones starting from a capital letter (e.g., data constructors),
-- symbol identifiers, and functions.
data LetterType
  = Capital
  | Symbol
  | Lower
  deriving (Eq, Ord)

-- | This function sorts import declarations and explicit imports in them
-- by their names.
sortImportsByName :: [WithComments Import] -> [WithComments Import]
sortImportsByName = fmap sortExplicitImportsInDecl . sortByModuleName

-- | This function sorts imports by their start line numbers.
sortImportsByLocation ::
     [GHC.LImportDecl GHC.GhcPs] -> [GHC.LImportDecl GHC.GhcPs]
sortImportsByLocation = sortBy (flip compare `on` lineIdx)
  where
    lineIdx = startLine . GHC.locA . GHC.getLoc

-- | This function sorts import declarations by their module names.
sortByModuleName :: [WithComments Import] -> [WithComments Import]
sortByModuleName =
  sortBy (compare `on` GHC.unLoc . GHC.ideclName . import' . getNode)

-- | This function sorts explicit imports in the given import declaration
-- by their names.
sortExplicitImportsInDecl :: WithComments Import -> WithComments Import
#if MIN_VERSION_ghc_lib_parser(9,6,1)
sortExplicitImportsInDecl = fmap f
  where
    f Import { import' = d@GHC.ImportDecl {ideclImportList = Just (x, imports)}
             , ..
             } =
      Import {import' = d {GHC.ideclImportList = Just (x, sorted)}, ..}
      where
        sorted = fmap (fmap sortVariants . sortExplicitImports) imports
    f x = x
#else
sortExplicitImportsInDecl = fmap f
  where
    f Import {import' = d@GHC.ImportDecl {ideclHiding = Just (x, imports)}, ..} =
      Import {import' = d {GHC.ideclHiding = Just (x, sorted)}, ..}
      where
        sorted = fmap (fmap sortVariants . sortExplicitImports) imports
    f x = x
#endif
-- | This function sorts the given explicit imports by their names.
sortExplicitImports :: [GHC.LIE GHC.GhcPs] -> [GHC.LIE GHC.GhcPs]
sortExplicitImports = sortBy compareImportEntities

-- | This function sorts variants (e.g., data constructors and class
-- methods) in the given explicit import by their names.
sortVariants :: GHC.LIE GHC.GhcPs -> GHC.LIE GHC.GhcPs
sortVariants (GHC.L l (GHC.IEThingWith x x' x'' xs)) =
  GHC.L l $ GHC.IEThingWith x x' x'' (sortWrappedNames xs)
  where
    sortWrappedNames = sortBy (compare `on` showOutputable)
sortVariants x = x

-- | This function compares two import declarations by their module names.
compareImportEntities :: GHC.LIE GHC.GhcPs -> GHC.LIE GHC.GhcPs -> Ordering
compareImportEntities (GHC.L _ a) (GHC.L _ b) =
  fromMaybe LT $ compareIdentifier <$> getModuleName a <*> getModuleName b

-- | This function returns a 'Just' value with the module name extracted
-- from the import declaration. Otherwise, it returns a 'Nothing'.
getModuleName :: GHC.IE GHC.GhcPs -> Maybe String
getModuleName (GHC.IEVar _ wrapped) = Just $ showOutputable wrapped
getModuleName (GHC.IEThingAbs _ wrapped) = Just $ showOutputable wrapped
getModuleName (GHC.IEThingAll _ wrapped) = Just $ showOutputable wrapped
getModuleName (GHC.IEThingWith _ wrapped _ _) = Just $ showOutputable wrapped
getModuleName _ = Nothing

-- | This function compares two identifiers in order of capitals, symbols,
-- and lowers.
compareIdentifier :: String -> String -> Ordering
compareIdentifier as@(a:_) bs@(b:_) =
  case compareChar a b of
    EQ -> compareSameIdentifierType as bs
    x -> x
compareIdentifier _ _ = error "Either identifier is an empty string."

-- | Almost similar to 'compare' but ignores parentheses for symbol
-- identifiers as they are enclosed by parentheses.
compareSameIdentifierType :: String -> String -> Ordering
compareSameIdentifierType "" "" = EQ
compareSameIdentifierType "" _ = LT
compareSameIdentifierType _ "" = GT
compareSameIdentifierType ('(':as) bs = compareSameIdentifierType as bs
compareSameIdentifierType (')':as) bs = compareSameIdentifierType as bs
compareSameIdentifierType as ('(':bs) = compareSameIdentifierType as bs
compareSameIdentifierType as (')':bs) = compareSameIdentifierType as bs
compareSameIdentifierType (a:as) (b:bs) =
  case compare a b of
    EQ -> compareSameIdentifierType as bs
    x -> x

-- | This function compares two characters by their types (capital, symbol,
-- and lower). If both are the same type, then it compares them by the
-- usual ordering.
compareChar :: Char -> Char -> Ordering
compareChar a b =
  case compare at bt of
    EQ -> compare a b
    x -> x
  where
    at = charToLetterType a
    bt = charToLetterType b

-- | This function returns a 'LetterType' based on the given character.
charToLetterType :: Char -> LetterType
charToLetterType c
  | isLower c = Lower
  | isUpper c = Capital
  | otherwise = Symbol

-- | This function returns the start line of the given 'SrcSpan'. If it is
-- not available, it raises an error.
startLine :: HasCallStack => GHC.SrcSpan -> Int
startLine (GHC.RealSrcSpan x _) = GHC.srcSpanStartLine x
startLine (GHC.UnhelpfulSpan _) = error "The src span is unavailable."
