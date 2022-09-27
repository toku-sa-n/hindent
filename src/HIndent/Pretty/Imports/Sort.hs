-- | Import declaration sorting for pretty-printing.
module HIndent.Pretty.Imports.Sort
  ( sortImportsByName
  , sortImportsByLocation
  ) where

import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.Pretty.Combinators

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
sortImportsByName :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortImportsByName = fmap sortExplicitImportsInDecl . sortModules

-- | This function sorts imports by their start line numbers.
sortImportsByLocation :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortImportsByLocation = sortBy (flip compare `on` lineIdx)
  where
    lineIdx = startLine . locA . getLoc

-- | This function sorts import declarations by their module names.
sortModules :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortModules = sortBy (compare `on` unLoc . ideclName . unLoc)

-- | This function sorts explicit imports in the given import declaration
-- by their names.
sortExplicitImportsInDecl :: LImportDecl GhcPs -> LImportDecl GhcPs
sortExplicitImportsInDecl (L l d@ImportDecl {ideclHiding = Just (x, imports)}) =
  L l d {ideclHiding = Just (x, sorted)}
  where
    sorted = fmap (fmap sortVariants . sortExplicitImports) imports
sortExplicitImportsInDecl x = x

-- | This function sorts the given explicit imports by their names.
sortExplicitImports :: [LIE GhcPs] -> [LIE GhcPs]
sortExplicitImports = sortBy compareImportEntities

sortVariants :: LIE GhcPs -> LIE GhcPs
sortVariants (L l (IEThingWith x x' x'' xs)) =
  L l $ IEThingWith x x' x'' (sortWrappedNames xs)
  where
    sortWrappedNames = sortBy (compare `on` showOutputable)
sortVariants x = x

compareImportEntities :: LIE GhcPs -> LIE GhcPs -> Ordering
compareImportEntities (L _ a) (L _ b) =
  fromMaybe LT $ compareIdentifier <$> moduleName a <*> moduleName b

moduleName :: IE GhcPs -> Maybe String
moduleName (IEVar _ wrapped)           = Just $ showOutputable wrapped
moduleName (IEThingAbs _ wrapped)      = Just $ showOutputable wrapped
moduleName (IEThingAll _ wrapped)      = Just $ showOutputable wrapped
moduleName (IEThingWith _ wrapped _ _) = Just $ showOutputable wrapped
moduleName _                           = Nothing

compareIdentifier :: String -> String -> Ordering
compareIdentifier [] _ = LT
compareIdentifier _ [] = GT
compareIdentifier (a:as) (b:bs) =
  case compareChar a b of
    EQ -> compareIdentifier as bs
    x  -> x

compareChar :: Char -> Char -> Ordering
compareChar a b =
  case compare at bt of
    EQ -> compare a b
    x  -> x
  where
    at = charToLetterType a
    bt = charToLetterType b

charToLetterType :: Char -> LetterType
charToLetterType c
  | isLower c = Lower
  | isUpper c = Capital
  | otherwise = Symbol

startLine :: SrcSpan -> Int
startLine (RealSrcSpan x _) = srcSpanStartLine x
startLine (UnhelpfulSpan _) = error "The src span is unavailable."
