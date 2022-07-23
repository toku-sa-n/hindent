module HIndent.Pretty.Imports.Sort
  ( sortImports
  , sortImportsByLocation
  ) where

import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.Pretty.Combinators

data LetterType
  = Capital
  | Symbol
  | Lower
  deriving (Eq, Ord)

sortImports :: [ImportDecl GhcPs] -> [ImportDecl GhcPs]
sortImports = fmap sortExplicitImportsInDecl . sortModules

sortImportsByLocation :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortImportsByLocation = sortBy (flip compare `on` lineIdx)
  where
    lineIdx x =
      case locA $ getLoc x of
        RealSrcSpan x' _ -> srcSpanStartLine x'
        _                -> error "Src span unavailable."

sortModules :: [ImportDecl GhcPs] -> [ImportDecl GhcPs]
sortModules = sortBy (compare `on` unLoc . ideclName)

sortExplicitImportsInDecl :: ImportDecl GhcPs -> ImportDecl GhcPs
sortExplicitImportsInDecl d@ImportDecl {ideclHiding = Nothing} = d
sortExplicitImportsInDecl d@ImportDecl {ideclHiding = Just (x, imports)} =
  d {ideclHiding = Just (x, sorted)}
  where
    sorted = fmap (fmap sortVariants . sortExplicitImports) imports

sortExplicitImports :: [LIE GhcPs] -> [LIE GhcPs]
sortExplicitImports = sortBy compareImportEntities

sortVariants :: LIE GhcPs -> LIE GhcPs
sortVariants (L l (IEThingWith x x' x'' xs)) =
  L l $ IEThingWith x x' x'' (sortWrappedNames xs)
  where
    sortWrappedNames =
      sortBy (\a b -> compare (showOutputable a) (showOutputable b))
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
