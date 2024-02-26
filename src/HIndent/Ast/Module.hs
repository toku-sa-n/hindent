-- | Module type.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module
  ( Module(..)
  , mkModule
  ) where

import Control.Monad.RWS
import Data.Maybe
import GHC.Hs hiding (comments)
import qualified GHC.Hs as GHC
import GHC.Types.SrcLoc
import HIndent.Ast.Import
import HIndent.Ast.Module.Declaration
import HIndent.Ast.Pragma
import HIndent.Ast.WithComments
import HIndent.Config
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.Import
import HIndent.Pretty.NodeComments
import HIndent.Printer
#if MIN_VERSION_ghc_lib_parser(9,6,1)
import GHC.Core.DataCon
#endif

#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type HsModule' = HsModule GHC.GhcPs
#else
type HsModule' = HsModule
#endif
data Module = Module
  { pragmas :: FileHeaderPragmaCollection
  , declaration :: Maybe ModuleDeclaration
  , imports :: ImportCollection
  , module' :: HsModule'
  }

instance CommentExtraction Module where
  nodeComments (Module {..}) = nodeComments module'
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance Pretty Module where
  pretty' m@Module { declaration = Nothing
                   , module' = HsModule {hsmodImports = [], hsmodDecls = []}
                   , ..
                   }
    | not (pragmaExists pragmas) = pure ()
  pretty' mo@Module {module' = m, ..} = blanklined printers >> newline
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists pragmas, pretty pragmas)
        , (moduleDeclExists, prettyModuleDecl mo)
        , (importsExist m, prettyImports)
        , (declsExist m, prettyDecls)
        ]
      moduleDeclExists = isJust declaration
      prettyDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp)
          $ addDeclSeparator
          $ hsmodDecls m
      prettyModuleDecl Module {declaration = Nothing} =
        error "The module declaration does not exist."
      prettyModuleDecl Module {declaration = Just d} = pretty d
      addDeclSeparator [] = []
      addDeclSeparator [x] = [(x, Nothing)]
      addDeclSeparator (x:xs) =
        (x, Just $ declSeparator $ unLoc x) : addDeclSeparator xs
      declSeparator (SigD _ TypeSig {}) = newline
      declSeparator (SigD _ InlineSig {}) = newline
      declSeparator (SigD _ PatSynSig {}) = newline
      declSeparator _ = blankline
      declsExist = not . null . hsmodDecls
      prettyImports = importDecls >>= blanklined . fmap outputImportGroup
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True -> pure $ extractImportsSorted m
          False -> pure $ extractImports m
#else
instance Pretty Module where
  pretty' Module { declaration = Nothing
                 , pragmas
                 , module' = HsModule {hsmodImports = [], hsmodDecls = []}
                 }
    | not (pragmaExists pragmas) = pure ()
  pretty' mo@Module {module' = m, ..} = blanklined printers >> newline
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists pragmas, pretty pragmas)
        , (moduleDeclExists, prettyModuleDecl mo)
        , (importsExist m, prettyImports)
        , (declsExist m, prettyDecls)
        ]
      prettyModuleDecl Module {declaration = Nothing} =
        error "The module declaration does not exist."
      prettyModuleDecl Module {declaration = Just d} = pretty d
      moduleDeclExists = isJust declaration
      prettyDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp)
          $ addDeclSeparator
          $ hsmodDecls m
      addDeclSeparator [] = []
      addDeclSeparator [x] = [(x, Nothing)]
      addDeclSeparator (x:xs) =
        (x, Just $ declSeparator $ unLoc x) : addDeclSeparator xs
      declSeparator (SigD _ TypeSig {}) = newline
      declSeparator (SigD _ InlineSig {}) = newline
      declSeparator (SigD _ PatSynSig {}) = newline
      declSeparator _ = blankline
      declsExist = not . null . hsmodDecls
      prettyImports = importDecls >>= blanklined . fmap outputImportGroup
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True -> pure $ extractImportsSorted m
          False -> pure $ extractImports m
#endif
mkModule :: HsModule' -> WithComments Module
mkModule m = mkWithCommentsWithEpAnn ann Module {..}
  where
    ann = getAnn m
    declaration = mkModuleDeclaration m
    pragmas = mkFileHeaderPragmaCollection m
    imports = mkImportCollection m
    module' = m

getAnn :: HsModule' -> EpAnn GHC.AnnsModule
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
getAnn = hsmodAnn . hsmodExt
#else
getAnn = hsmodAnn
#endif
