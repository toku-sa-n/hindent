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
import           HIndent.SrcSpan

data LetterType
  = Capital
  | Symbol
  | Lower
  deriving (Eq, Ord)

sortImportsByName :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortImportsByName = fmap sortExplicitImportsInDecl . sortModules

sortImportsByLocation :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortImportsByLocation = sortBy (flip compare `on` lineIdx)
  where
    lineIdx = startLine . locA . getLoc

sortModules :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortModules = sortBy (compare `on` unLoc . ideclName . unLoc)

sortExplicitImportsInDecl :: LImportDecl GhcPs -> LImportDecl GhcPs
sortExplicitImportsInDecl (L l d@ImportDecl {ideclHiding = Nothing}) = L l d
sortExplicitImportsInDecl (L l d@ImportDecl {ideclHiding = Just (x, imports)}) =
  L l d {ideclHiding = Just (x, sorted)}
  where
    sorted = fmap (fmap sortVariants . sortExplicitImports) imports

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
  fromMaybe LT $ do
    a' <- moduleName a
    b' <- moduleName b
    return $ compareIdentifier a' b'

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