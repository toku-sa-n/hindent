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
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type HsModule' = HsModule GhcPs
#else
type HsModule' = HsModule
#endif
data Module = Module
  { pragmas :: FileHeaderPragmaCollection
  , declaration :: Maybe ModuleDeclaration
  , imports :: ImportCollection
  , declarations :: DeclarationCollection
  , module' :: HsModule'
  }

instance CommentExtraction Module where
  nodeComments (Module {..}) = nodeComments module'

instance Pretty Module where
  pretty' Module {declaration = Nothing, pragmas, imports, declarations}
    | not (pragmaExists pragmas)
        && not (hasImports imports)
        && not (hasDeclarations declarations) = pure ()
  pretty' mo@Module {..} = blanklined printers >> newline
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists pragmas, pretty pragmas)
        , (moduleDeclExists, prettyModuleDecl mo)
        , (hasImports imports, pretty imports)
        , (hasDeclarations declarations, pretty declarations)
        ]
      prettyModuleDecl Module {declaration = Nothing} =
        error "The module declaration does not exist."
      prettyModuleDecl Module {declaration = Just d} = pretty d
      moduleDeclExists = isJust declaration

mkModule :: HsModule' -> WithComments Module
mkModule m = mkWithCommentsWithEpAnn ann Module {..}
  where
    ann = getAnn m
    declaration = mkModuleDeclaration m
    pragmas = mkFileHeaderPragmaCollection m
    imports = mkImportCollection m
    declarations = mkDeclarationCollection m
    module' = m

getAnn :: HsModule' -> EpAnn AnnsModule
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
getAnn = hsmodAnn . hsmodExt
#else
getAnn = hsmodAnn
#endif
