module HIndent.Pretty.Imports.Sort
  ( sortImports
  ) where

import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.Pretty.Combinators

sortImports :: [ImportDecl GhcPs] -> [ImportDecl GhcPs]
sortImports = fmap sortExplicitImportsInDecl . sortModules

sortModules :: [ImportDecl GhcPs] -> [ImportDecl GhcPs]
sortModules = sortBy (compare `on` unLoc . ideclName)

sortExplicitImportsInDecl :: ImportDecl GhcPs -> ImportDecl GhcPs
sortExplicitImportsInDecl d@ImportDecl {ideclHiding = Nothing} = d
sortExplicitImportsInDecl d@ImportDecl {ideclHiding = Just (x, imports)} =
  d
    { ideclHiding =
        Just (x, fmap (fmap sortVariants . sortExplicitImports) imports)
    }

sortVariants :: LIE GhcPs -> LIE GhcPs
sortVariants (L l (IEThingWith x x' x'' xs)) =
  L l $ IEThingWith x x' x'' (sortWrappedNames xs)
  where
    sortWrappedNames =
      sortBy (\a b -> compare (showOutputable a) (showOutputable b))
sortVariants x = x

sortExplicitImports :: [LIE GhcPs] -> [LIE GhcPs]
sortExplicitImports = sortBy compareImportEntities

compareImportEntities :: LIE GhcPs -> LIE GhcPs -> Ordering
compareImportEntities (L _ a) (L _ b) =
  fromMaybe LT $ do
    a' <- moduleName a
    b' <- moduleName b
    return $ compareModuleName a' b'

moduleName :: IE GhcPs -> Maybe String
moduleName (IEVar _ wrapped)           = Just $ showOutputable wrapped
moduleName (IEThingAbs _ wrapped)      = Just $ showOutputable wrapped
moduleName (IEThingAll _ wrapped)      = Just $ showOutputable wrapped
moduleName (IEThingWith _ wrapped _ _) = Just $ showOutputable wrapped
moduleName _                           = Nothing

data LetterType
  = Capital
  | Symbol
  | Lower
  deriving (Eq, Ord)

compareModuleName :: String -> String -> Ordering
compareModuleName [] _ = LT
compareModuleName _ [] = GT
compareModuleName (a:as) (b:bs) =
  case compareChar a b of
    EQ -> compareModuleName as bs
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
