{-# LANGUAGE CPP #-}

module HIndent.Ast.ValueLiteral
  ( LiteralValue
  , OverloadedValue
  , mkLiteralValue
  , mkOverloadedValue
  ) where

import qualified GHC.Data.FastString as GHC
import qualified GHC.Types.SourceText as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data LiteralValue
  = InlineLiteral String
  | MultilineStringLiteral [String]

data OverloadedValue
  = IntegralValue String
  | FractionalValue String
  | StringValue String

instance Pretty LiteralValue where
  pretty' (InlineLiteral value) = string value
  pretty' (MultilineStringLiteral []) = pure ()
  pretty' (MultilineStringLiteral [value]) = string value
  pretty' (MultilineStringLiteral (value:values)) = do
    string value
    newline
    indentedWithFixedLevel 0 $ lined $ fmap string values

instance Pretty OverloadedValue where
  pretty' (IntegralValue value) = string value
  pretty' (FractionalValue value) = string value
  pretty' (StringValue value) = string value

mkLiteralValue :: GHC.HsLit GHC.GhcPs -> LiteralValue
mkLiteralValue x@(GHC.HsChar _ _) = InlineLiteral $ showOutputable x
mkLiteralValue x@GHC.HsCharPrim {} = InlineLiteral $ showOutputable x
mkLiteralValue GHC.HsInt {} = notUsedInParsedStage
mkLiteralValue (GHC.HsIntPrim _ value) = InlineLiteral $ show value ++ "#"
mkLiteralValue GHC.HsWordPrim {} = notUsedInParsedStage
mkLiteralValue GHC.HsInt64Prim {} = notUsedInParsedStage
mkLiteralValue GHC.HsWord64Prim {} = notUsedInParsedStage
#if !MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkLiteralValue GHC.HsInteger {} = notUsedInParsedStage
mkLiteralValue GHC.HsRat {} = notUsedInParsedStage
#endif
mkLiteralValue (GHC.HsFloatPrim _ value) =
  InlineLiteral $ showOutputable value ++ "#"
mkLiteralValue GHC.HsDoublePrim {} = notUsedInParsedStage
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
mkLiteralValue GHC.HsInt8Prim {} = notUsedInParsedStage
mkLiteralValue GHC.HsInt16Prim {} = notUsedInParsedStage
mkLiteralValue GHC.HsInt32Prim {} = notUsedInParsedStage
mkLiteralValue GHC.HsWord8Prim {} = notUsedInParsedStage
mkLiteralValue GHC.HsWord16Prim {} = notUsedInParsedStage
mkLiteralValue GHC.HsWord32Prim {} = notUsedInParsedStage
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkLiteralValue GHC.HsMultilineString {} = notGeneratedByParser
#endif
mkLiteralValue x =
  case lines $ showOutputable x of
    [] -> InlineLiteral ""
    [value] -> InlineLiteral value
    values -> MultilineStringLiteral values

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
notGeneratedByParser :: a
notGeneratedByParser = error "`ghc-lib-parser` never generates this AST node."

notUsedInParsedStage :: a
notUsedInParsedStage =
  error
    "This AST should never appears in an AST. It only appears in the renaming or type checked stages."
