{-# LANGUAGE RecordWildCards #-}

module HIndent.Pretty.Imports
  ( outputImports
  , importsExist
  ) where

import           Control.Monad
import           Data.Function
import           Data.List
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.Pretty.Combinators
import           HIndent.Types

outputImports :: HsModule -> Printer ()
outputImports m =
  forM_ (sortImports $ unLoc <$> hsmodImports m) $ \x -> do
    outputImport x
    newline

importsExist :: HsModule -> Bool
importsExist = not . null . hsmodImports

sortImports :: [ImportDecl GhcPs] -> [ImportDecl GhcPs]
sortImports = sortBy (compare `on` unLoc . ideclName)

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
