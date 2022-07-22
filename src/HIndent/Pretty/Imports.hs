{-# LANGUAGE RecordWildCards #-}

module HIndent.Pretty.Imports
  ( outputImports
  , importsExist
  ) where

import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.Pretty.Combinators
import           HIndent.Types

outputImports :: HsModule -> Printer ()
outputImports m =
  forM_ (sortModulesAndExplicitImports $ unLoc <$> hsmodImports m) $ \x -> do
    outputImport x
    newline

importsExist :: HsModule -> Bool
importsExist = not . null . hsmodImports

outputImport :: ImportDecl GhcPs -> Printer ()
outputImport ImportDecl {..} = do
  string "import "
  unless (ideclQualified == NotQualified) $ string "qualified "
  outputOutputable ideclName
  case ideclAs of
    Just x -> do
      string " as "
      outputOutputable x
    Nothing -> return ()
  case ideclHiding of
    Nothing -> return ()
    Just (x, _) -> do
      string
        (if x
           then " hiding "
           else " ")
      horizontalOrVerticalTuple $ fmap outputOutputable explicitOrHidingImports
  where
    explicitOrHidingImports = maybe [] (fmap unLoc . unLoc . snd) ideclHiding

sortModulesAndExplicitImports :: [ImportDecl GhcPs] -> [ImportDecl GhcPs]
sortModulesAndExplicitImports = fmap sortExplicitImportsInDecl . sortModules

sortModules :: [ImportDecl GhcPs] -> [ImportDecl GhcPs]
sortModules = sortBy (compare `on` unLoc . ideclName)

sortExplicitImportsInDecl :: ImportDecl GhcPs -> ImportDecl GhcPs
sortExplicitImportsInDecl d@ImportDecl {ideclHiding = Nothing} = d
sortExplicitImportsInDecl d@ImportDecl {ideclHiding = Just (x, imports)} =
  d {ideclHiding = Just (x, fmap sortExplicitImports imports)}

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
