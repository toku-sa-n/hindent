{-# LANGUAGE RecordWildCards #-}

module HIndent.Pretty.Imports
  ( outputImports
  , importsExist
  ) where

import           Control.Monad
import           GHC.Hs
import           GHC.Types.SrcLoc
import           GHC.Unit
import           HIndent.Applicative
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Imports.Sort
import           HIndent.Types

outputImports :: HsModule -> Printer ()
outputImports =
  inter blankline .
  fmap (outputImportGroup . sortImportsByName . fmap unLoc) .
  groupImports . sortImportsByLocation . hsmodImports

outputImportGroup :: [ImportDecl GhcPs] -> Printer ()
outputImportGroup = inter newline . fmap outputImport

importsExist :: HsModule -> Bool
importsExist = not . null . hsmodImports

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
      srcSpanEndLine (sp a) + 1 == srcSpanStartLine (sp b) ||
      srcSpanEndLine (sp b) + 1 == srcSpanStartLine (sp a)
    sp x =
      case locA $ getLoc x of
        RealSrcSpan x' _ -> x'
        _                -> error "Src span unavailable."

outputImport :: ImportDecl GhcPs -> Printer ()
outputImport ImportDecl {..} = do
  string "import "
  when (ideclSource == IsBoot) $ string "{-# SOURCE #-} "
  when ideclSafe $ string "safe "
  unless (ideclQualified == NotQualified) $ string "qualified "
  output ideclName
  whenJust ideclAs $ \x -> do
    string " as "
    output x
  whenJust ideclHiding $ \(x, _) -> do
    when x (string " hiding")
    (string " " >> horizontalTuple explicitOrHidingImports) `ifFitsOnOneLineOrElse`
      (newline >> indentedBlock (verticalTuple explicitOrHidingImports))
  where
    explicitOrHidingImports =
      output <$> maybe [] (fmap unLoc . unLoc . snd) ideclHiding
