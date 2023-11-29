-- | Module type.
{-# LANGUAGE CPP #-}

module HIndent.Ast.Module
  ( Module(..)
  , mkModule
  , pragmaExists
  ) where

import qualified GHC.Hs                as GHC
import qualified HIndent.Pretty.Pragma as Pretty
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
newtype Module =
  Module (GHC.HsModule GHC.GhcPs)
#else
newtype Module =
  Module GHC.HsModule
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkModule :: GHC.HsModule GHC.GhcPs -> Module
#else
mkModule :: GHC.HsModule -> Module
#endif
mkModule = Module

pragmaExists :: Module -> Bool
pragmaExists (Module m) = Pretty.pragmaExists m
