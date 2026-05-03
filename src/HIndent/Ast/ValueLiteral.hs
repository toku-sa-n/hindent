{-# LANGUAGE CPP #-}

module HIndent.Ast.ValueLiteral
  ( ValueLiteral
  , mkValueLiteralFromHsLit
  , mkValueLiteralFromHsOverLit
  ) where

import qualified GHC.Data.FastString as GHC
import qualified GHC.Types.SourceText as GHC
import HIndent.Ast.TextValue
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data ValueLiteral
  = InlineLiteral TextValue
  | MultilineStringLiteral [TextValue]

instance Pretty ValueLiteral where
  pretty (InlineLiteral value) = pretty value
  pretty (MultilineStringLiteral []) = pure ()
  pretty (MultilineStringLiteral [value]) = pretty value
  pretty (MultilineStringLiteral (value:values)) = do
    pretty value
    newline
    indentedWithFixedLevel 0 $ lined $ fmap pretty values

mkValueLiteralFromHsLit :: GHC.HsLit GHC.GhcPs -> ValueLiteral
mkValueLiteralFromHsLit x@(GHC.HsChar _ _) =
  InlineLiteral $ mkTextValueFromString $ showOutputable x
mkValueLiteralFromHsLit x@GHC.HsCharPrim {} =
  InlineLiteral $ mkTextValueFromString $ showOutputable x
mkValueLiteralFromHsLit GHC.HsInt {} = notUsedInParsedStage
mkValueLiteralFromHsLit (GHC.HsIntPrim _ value) =
  InlineLiteral $ mkTextValueFromString $ show value ++ "#"
mkValueLiteralFromHsLit GHC.HsWordPrim {} = notUsedInParsedStage
mkValueLiteralFromHsLit GHC.HsInt64Prim {} = notUsedInParsedStage
mkValueLiteralFromHsLit GHC.HsWord64Prim {} = notUsedInParsedStage
#if !MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkValueLiteralFromHsLit GHC.HsInteger {} = notUsedInParsedStage
mkValueLiteralFromHsLit GHC.HsRat {} = notUsedInParsedStage
#endif
mkValueLiteralFromHsLit (GHC.HsFloatPrim _ value) =
  InlineLiteral $ mkTextValueFromString $ showOutputable value ++ "#"
mkValueLiteralFromHsLit GHC.HsDoublePrim {} = notUsedInParsedStage
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
mkValueLiteralFromHsLit GHC.HsInt8Prim {} = notUsedInParsedStage
mkValueLiteralFromHsLit GHC.HsInt16Prim {} = notUsedInParsedStage
mkValueLiteralFromHsLit GHC.HsInt32Prim {} = notUsedInParsedStage
mkValueLiteralFromHsLit GHC.HsWord8Prim {} = notUsedInParsedStage
mkValueLiteralFromHsLit GHC.HsWord16Prim {} = notUsedInParsedStage
mkValueLiteralFromHsLit GHC.HsWord32Prim {} = notUsedInParsedStage
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkValueLiteralFromHsLit GHC.HsMultilineString {} = notGeneratedByParser
#endif
mkValueLiteralFromHsLit x =
  case lines $ showOutputable x of
    [] -> InlineLiteral $ mkTextValueFromString ""
    [value] -> InlineLiteral $ mkTextValueFromString value
    values -> MultilineStringLiteral $ fmap mkTextValueFromString values

mkValueLiteralFromHsOverLit :: GHC.HsOverLit GHC.GhcPs -> ValueLiteral
mkValueLiteralFromHsOverLit GHC.OverLit {GHC.ol_val = value} =
  case value of
    GHC.HsIntegral integralLiteral ->
      InlineLiteral $ renderIntegral integralLiteral
    GHC.HsFractional fractionalLiteral ->
      InlineLiteral $ mkTextValueFromString $ showOutputable fractionalLiteral
    GHC.HsIsString _ stringValue ->
      InlineLiteral $ mkTextValueFromString $ GHC.unpackFS stringValue

renderIntegral :: GHC.IntegralLit -> TextValue
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
renderIntegral GHC.IL {GHC.il_text = GHC.SourceText value} =
  mkTextValueFromString $ GHC.unpackFS value
renderIntegral GHC.IL {GHC.il_value = value} =
  mkTextValueFromString $ show value
#else
renderIntegral GHC.IL {GHC.il_text = GHC.SourceText value} =
  mkTextValueFromString value
renderIntegral GHC.IL {GHC.il_value = value} =
  mkTextValueFromString $ show value
#endif

#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
notGeneratedByParser :: a
notGeneratedByParser = error "`ghc-lib-parser` never generates this AST node."
#endif
notUsedInParsedStage :: a
notUsedInParsedStage =
  error
    "This AST should never appears in an AST. It only appears in the renaming or type checked stages."
