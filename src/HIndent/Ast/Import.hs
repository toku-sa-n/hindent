{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import
  ( ImportCollection
  , mkImportCollection
  , hasImports
  ) where

import Control.Monad.RWS
import GHC.Hs
import GHC.Types.SrcLoc
import HIndent.Config
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.Import.Sort
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
import HIndent.Printer

newtype ImportCollection =
  ImportCollection [Import]

instance CommentExtraction ImportCollection where
  nodeComments (ImportCollection _) = NodeComments [] [] []

instance Pretty ImportCollection where
  pretty' (ImportCollection imports) = prettyImports
    where
      prettyImports = importDecls >>= blanklined . fmap outputImportGroup
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True -> pure $ extractImportsSorted' $ fmap import' imports
          False -> pure $ extractImports' $ fmap import' imports

data Import = Import
  { isSafeImport :: Bool
  , import' :: LImportDecl GhcPs
  }
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkImportCollection :: HsModule GhcPs -> ImportCollection
#else
mkImportCollection :: HsModule -> ImportCollection
#endif
mkImportCollection HsModule {..} = ImportCollection $ fmap mkImport hsmodImports

mkImport :: LImportDecl GhcPs -> Import
mkImport import' = Import {isSafeImport = True, import'}

hasImports :: ImportCollection -> Bool
hasImports (ImportCollection imports) = not $ null imports

extractImports' :: [LImportDecl GhcPs] -> [[LImportDecl GhcPs]]
extractImports' = groupImports . sortImportsByLocation

extractImportsSorted' :: [LImportDecl GhcPs] -> [[LImportDecl GhcPs]]
extractImportsSorted' = fmap sortImportsByName . extractImports'

-- | Combines adjacent import declarations into a single list.
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
      srcSpanEndLine (sp a) + 1 == srcSpanStartLine (sp b)
        || srcSpanEndLine (sp b) + 1 == srcSpanStartLine (sp a)
    sp x =
      case locA $ getLoc x of
        RealSrcSpan x' _ -> x'
        _ -> error "Src span unavailable."
