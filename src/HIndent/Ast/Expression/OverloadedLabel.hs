{-# LANGUAGE CPP #-}

module HIndent.Ast.Expression.OverloadedLabel
  ( OverloadedLabel
  , mkOverloadedLabel
  ) where

import qualified GHC.Data.FastString as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype OverloadedLabel =
  OverloadedLabel String

instance Pretty OverloadedLabel where
  pretty (OverloadedLabel s) = string "#" >> string s

mkOverloadedLabel :: GHC.FastString -> OverloadedLabel
mkOverloadedLabel fs = OverloadedLabel $ GHC.unpackFS fs
