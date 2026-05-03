{-# LANGUAGE CPP #-}

module HIndent.Ast.StringLiteral
  ( StringLiteral
  , mkStringLiteral
  ) where

import qualified GHC.Types.SourceText as GHC
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
import qualified GHC.Data.FastString as GHC
#endif
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype StringLiteral =
  StringLiteral String

instance Pretty StringLiteral where
  pretty (StringLiteral value) = string value

mkStringLiteral :: GHC.StringLiteral -> StringLiteral
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkStringLiteral GHC.StringLiteral {GHC.sl_st = GHC.SourceText value} =
  StringLiteral $ GHC.unpackFS value
mkStringLiteral GHC.StringLiteral {GHC.sl_fs = value} =
  StringLiteral $ GHC.unpackFS value
#else
mkStringLiteral literal = StringLiteral $ showOutputable literal
#endif
