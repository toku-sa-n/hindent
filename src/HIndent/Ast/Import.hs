-- TODO: Split this file into multiple files
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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
import GHC.Hs
import GHC.Stack
import GHC.Types.SrcLoc
import HIndent.Ast.WithComments
import HIndent.Config
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
import HIndent.Printer

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
  { isSafeImport :: Bool
  , import' :: ImportDecl GhcPs
  }

instance CommentExtraction Import where
  nodeComments Import {} = NodeComments [] [] []

instance Pretty Import where
  pretty' Import {..} = pretty import'
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkImportCollection :: HsModule GhcPs -> ImportCollection
#else
mkImportCollection :: HsModule -> ImportCollection
#endif
mkImportCollection HsModule {..} =
  ImportCollection
    $ fmap (fmap mkImport . mkWithCommentsWithGenLocated)
        <$> extractImports hsmodImports

mkImport :: ImportDecl GhcPs -> Import
mkImport import' = Import {isSafeImport = ideclSafe import', import'}

hasImports :: ImportCollection -> Bool
hasImports (ImportCollection imports) = not $ null imports

extractImports :: [LImportDecl GhcPs] -> [[LImportDecl GhcPs]]
extractImports = groupImports . sortImportsByLocation

-- | Combines adjacent import declarations into a single list.
groupImports :: [LImportDecl GhcPs] -> [[LImportDecl GhcPs]]
groupImports = groupImports' []
  where
    groupImports' ::
         [[LImportDecl GhcPs]] -> [LImportDecl GhcPs] -> [[LImportDecl GhcPs]]
    groupImports' xs [] = xs
    groupImports' [] (x:xs) = groupImports' [[x]] xs
    groupImports' [[]] (x:xs) = groupImports' [[x]] xs
    groupImports' ([]:x:xs) (y:ys) = groupImports' ([y] : x : xs) ys
    groupImports' ((z:zs):xs) (y:ys)
      | z `isAdjacentTo` y = groupImports' ((y : z : zs) : xs) ys
      | otherwise = groupImports' ([y] : (z : zs) : xs) ys
    a `isAdjacentTo` b =
      srcSpanEndLine (sp a) + 1 == srcSpanStartLine (sp b)
        || srcSpanEndLine (sp b) + 1 == srcSpanStartLine (sp a)
    sp x =
      case locA $ getLoc x of
        RealSrcSpan x' _ -> x'
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
sortImportsByLocation :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortImportsByLocation = sortBy (flip compare `on` lineIdx)
  where
    lineIdx = startLine . locA . getLoc

-- | This function sorts import declarations by their module names.
sortByModuleName :: [WithComments Import] -> [WithComments Import]
sortByModuleName = sortBy (compare `on` unLoc . ideclName . import' . getNode)

-- | This function sorts explicit imports in the given import declaration
-- by their names.
sortExplicitImportsInDecl :: WithComments Import -> WithComments Import
#if MIN_VERSION_ghc_lib_parser(9,6,1)
sortExplicitImportsInDecl = fmap f
  where
    f Import {import' = d@ImportDecl {ideclImportList = Just (x, imports)}, ..} =
      Import {import' = d {ideclImportList = Just (x, sorted)}, ..}
      where
        sorted = fmap (fmap sortVariants . sortExplicitImports) imports
    f x = x
#else
sortExplicitImportsInDecl = fmap f
  where
    f Import {import' = d@ImportDecl {ideclHiding = Just (x, imports)}, ..} =
      Import {import' = d {ideclHiding = Just (x, sorted)}, ..}
      where
        sorted = fmap (fmap sortVariants . sortExplicitImports) imports
    f x = x
#endif
-- | This function sorts the given explicit imports by their names.
sortExplicitImports :: [LIE GhcPs] -> [LIE GhcPs]
sortExplicitImports = sortBy compareImportEntities

-- | This function sorts variants (e.g., data constructors and class
-- methods) in the given explicit import by their names.
sortVariants :: LIE GhcPs -> LIE GhcPs
sortVariants (L l (IEThingWith x x' x'' xs)) =
  L l $ IEThingWith x x' x'' (sortWrappedNames xs)
  where
    sortWrappedNames = sortBy (compare `on` showOutputable)
sortVariants x = x

-- | This function compares two import declarations by their module names.
compareImportEntities :: LIE GhcPs -> LIE GhcPs -> Ordering
compareImportEntities (L _ a) (L _ b) =
  fromMaybe LT $ compareIdentifier <$> moduleName a <*> moduleName b

-- | This function returns a 'Just' value with the module name extracted
-- from the import declaration. Otherwise, it returns a 'Nothing'.
moduleName :: IE GhcPs -> Maybe String
moduleName (IEVar _ wrapped) = Just $ showOutputable wrapped
moduleName (IEThingAbs _ wrapped) = Just $ showOutputable wrapped
moduleName (IEThingAll _ wrapped) = Just $ showOutputable wrapped
moduleName (IEThingWith _ wrapped _ _) = Just $ showOutputable wrapped
moduleName _ = Nothing

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
startLine :: HasCallStack => SrcSpan -> Int
startLine (RealSrcSpan x _) = srcSpanStartLine x
startLine (UnhelpfulSpan _) = error "The src span is unavailable."
