{-# LANGUAGE CPP #-}

module HIndent.Ast.ValueLiteral.OverloadedValue
  ( OverloadedValue
  , mkOverloadedValue
  ) where

import qualified GHC.Data.FastString as GHC
import qualified GHC.Types.SourceText as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data OverloadedValue
  = IntegralValue String
  | FractionalValue String
  | StringValue String

instance Pretty OverloadedValue where
  pretty (IntegralValue value) = string value
  pretty (FractionalValue value) = string value
  pretty (StringValue value) = string value

mkOverloadedValue :: GHC.HsOverLit GHC.GhcPs -> OverloadedValue
mkOverloadedValue GHC.OverLit {GHC.ol_val = value} =
  case value of
    GHC.HsIntegral integralLiteral ->
      IntegralValue $ renderIntegral integralLiteral
    GHC.HsFractional fractionalLiteral ->
      FractionalValue $ showOutputable fractionalLiteral
    GHC.HsIsString _ stringValue -> StringValue $ GHC.unpackFS stringValue

renderIntegral :: GHC.IntegralLit -> String
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
renderIntegral GHC.IL {GHC.il_text = GHC.SourceText value} = GHC.unpackFS value
renderIntegral GHC.IL {GHC.il_value = value} = show value
#else
renderIntegral GHC.IL {GHC.il_text = GHC.SourceText value} = value
renderIntegral GHC.IL {GHC.il_value = value} = show value
#endif
