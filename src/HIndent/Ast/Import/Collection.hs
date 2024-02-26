{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import.Collection
  ( ImportCollection
  , mkImportCollection
  , hasImports
  ) where

import Control.Monad.RWS
import Data.Function
import Data.List
import qualified GHC.Hs as GHC
import GHC.Stack
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Import
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
          True -> pure $ fmap sortImportsAndEntriesByName imports
          False -> pure imports
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkImportCollection :: GHC.HsModule GHC.GhcPs -> ImportCollection
#else
mkImportCollection :: GHC.HsModule -> ImportCollection
#endif
mkImportCollection GHC.HsModule {..} =
  ImportCollection
    $ fmap (fmap mkImport . mkWithCommentsWithGenLocated)
        <$> extractImports hsmodImports

hasImports :: ImportCollection -> Bool
hasImports (ImportCollection imports) = not $ null imports

extractImports :: [GHC.LImportDecl GHC.GhcPs] -> [[GHC.LImportDecl GHC.GhcPs]]
extractImports = groupImports . sortImportsByLocation

-- | Combines adjacent import declarations into a single list.
groupImports :: [GHC.LImportDecl GHC.GhcPs] -> [[GHC.LImportDecl GHC.GhcPs]]
groupImports = groupImports' []
  where
    groupImports' ::
         [[GHC.LImportDecl GHC.GhcPs]]
      -> [GHC.LImportDecl GHC.GhcPs]
      -> [[GHC.LImportDecl GHC.GhcPs]]
    groupImports' xs [] = xs
    groupImports' [] (x:xs) = groupImports' [[x]] xs
    groupImports' [[]] (x:xs) = groupImports' [[x]] xs
    groupImports' ([]:x:xs) (y:ys) = groupImports' ([y] : x : xs) ys
    groupImports' ((z:zs):xs) (y:ys)
      | z `isAdjacentTo` y = groupImports' ((y : z : zs) : xs) ys
      | otherwise = groupImports' ([y] : (z : zs) : xs) ys
    a `isAdjacentTo` b =
      GHC.srcSpanEndLine (sp a) + 1 == GHC.srcSpanStartLine (sp b)
        || GHC.srcSpanEndLine (sp b) + 1 == GHC.srcSpanStartLine (sp a)
    sp x =
      case GHC.locA $ GHC.getLoc x of
        GHC.RealSrcSpan x' _ -> x'
        _ -> error "Src span unavailable."

-- | This function sorts imports by their start line numbers.
sortImportsByLocation ::
     [GHC.LImportDecl GHC.GhcPs] -> [GHC.LImportDecl GHC.GhcPs]
sortImportsByLocation = sortBy (flip compare `on` lineIdx)
  where
    lineIdx = startLine . GHC.locA . GHC.getLoc

-- | This function returns the start line of the given 'SrcSpan'. If it is
-- not available, it raises an error.
startLine :: HasCallStack => GHC.SrcSpan -> Int
startLine (GHC.RealSrcSpan x _) = GHC.srcSpanStartLine x
startLine (GHC.UnhelpfulSpan _) = error "The src span is unavailable."
