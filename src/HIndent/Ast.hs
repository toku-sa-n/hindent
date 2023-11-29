-- | AST type and its components.
--
-- The Abstract Syntax Tree (AST) provided by ghc-lib-parser, specifically
-- HsModule and its components, undergoes frequent changes with each update of
-- the library. To accommodate the differences between multiple versions of the
-- same library, HIndent defines its own custom AST type.
{-# LANGUAGE CPP #-}

module HIndent.Ast
  ( module HIndent.Ast.Module
  ) where

import           HIndent.Ast.Module
