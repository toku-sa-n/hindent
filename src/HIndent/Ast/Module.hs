-- | Module type.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module
  ( Module(..)
  , mkModule
  ) where

import Data.Maybe
import GHC.Hs hiding (comments)
import HIndent.Ast.Declaration
import HIndent.Ast.Import
import HIndent.Ast.Module.Declaration
import HIndent.Ast.Pragma
import HIndent.Ast.WithComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type HsModule' = HsModule GhcPs
#else
type HsModule' = HsModule
#endif
data Module = Module
  { pragmas :: FileHeaderPragmaCollection
  , moduleDeclaration :: Maybe ModuleDeclaration
  , imports :: ImportCollection
  , declarations :: DeclarationCollection
  }

instance CommentExtraction Module where
  nodeComments (Module {}) = NodeComments [] [] []

instance Pretty Module where
  pretty' Module {..}
    | not (pragmaExists pragmas)
        && isNothing moduleDeclaration
        && not (hasImports imports)
        && not (hasDeclarations declarations) = pure ()
    | otherwise = blanklined printers >> newline
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists pragmas, pretty pragmas)
        , (moduleDeclExists, prettyModuleDecl moduleDeclaration)
        , (hasImports imports, pretty imports)
        , (hasDeclarations declarations, pretty declarations)
        ]
      prettyModuleDecl Nothing = error "The module declaration does not exist."
      prettyModuleDecl (Just d) = pretty d
      moduleDeclExists = isJust moduleDeclaration

mkModule :: HsModule' -> WithComments Module
mkModule m = mkWithCommentsWithEpAnn ann Module {..}
  where
    ann = getAnn m
    moduleDeclaration = mkModuleDeclaration m
    pragmas = mkFileHeaderPragmaCollection m
    imports = mkImportCollection m
    declarations = mkDeclarationCollection m

getAnn :: HsModule' -> EpAnn AnnsModule
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
getAnn = hsmodAnn . hsmodExt
#else
getAnn = hsmodAnn
#endif
