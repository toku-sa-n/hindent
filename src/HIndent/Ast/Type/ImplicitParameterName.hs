{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Type.ImplicitParameterName
  ( ImplicitParameterName
  , mkImplicitParameterName
  ) where

import qualified GHC.Data.FastString as GHC
import HIndent.Ast.TextValue
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators (string)

newtype ImplicitParameterName =
  ImplicitParameterName TextValue

instance Pretty ImplicitParameterName where
  pretty (ImplicitParameterName s) = string "?" >> pretty s

mkImplicitParameterName :: GHC.HsIPName -> ImplicitParameterName
mkImplicitParameterName (GHC.HsIPName fs) =
  ImplicitParameterName $ mkTextValueFromString $ GHC.unpackFS fs
