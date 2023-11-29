-- | Module type.
{-# LANGUAGE CPP #-}

module HIndent.Ast.Module
  ( Module
  ) where

import qualified GHC.Hs as GHC
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type Module = (GHC.HsModule GHC.GhcPs)
#else
type Module = GHC.HsModule
#endif
