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
  whenJust ideclAs $ \x -> do
    string " as "
    outputOutputable x
  whenJust ideclHiding $ \(x, _) -> do
    when x (string " hiding")
    (string " " >> horizontalTuple explicitOrHidingImports) `ifFitsOnOneLineOrElse`
      (newline >> indentedBlock (verticalTuple explicitOrHidingImports))
  where
    explicitOrHidingImports =
      outputOutputable <$> maybe [] (fmap unLoc . unLoc . snd) ideclHiding

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _  = pure ()
whenJust (Just x) f = f x
