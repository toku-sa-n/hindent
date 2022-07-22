{-# LANGUAGE RecordWildCards #-}

module HIndent.Pretty.Imports
  ( outputImports
  , importsExist
  ) where

import           Control.Monad
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Imports.Sort
import           HIndent.Types

outputImports :: HsModule -> Printer ()
outputImports m =
  forM_ (sortImports $ unLoc <$> hsmodImports m) $ \x -> do
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
      when x (string " hiding")
      (string " " >>
       horizontalTuple (fmap outputOutputable explicitOrHidingImports)) `ifFitsOnOneLineOrElse`
        (newline >>
         indentedBlock
           (verticalTuple (fmap outputOutputable explicitOrHidingImports)))
  where
    explicitOrHidingImports = maybe [] (fmap unLoc . unLoc . snd) ideclHiding
