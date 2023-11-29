-- | AST type and its components.
--
-- The Abstract Syntax Tree (AST) provided by ghc-lib-parser, specifically
-- HsModule and its components, undergoes frequent changes with each update of
-- the library. To accommodate the differences between multiple versions of the
-- same library, HIndent defines its own custom AST type.
{-# LANGUAGE CPP #-}

module HIndent.Ast
  ( Ast
  ) where

import qualified GHC.Hs as GHC
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type Ast = GHC.HsModule GHC.GhcPs
#else
type Ast = GHC.HsModule
#endif
