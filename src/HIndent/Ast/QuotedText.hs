module HIndent.Ast.QuotedText
  ( QuotedText
  , mkQuotedText
  ) where

import qualified GHC.Types.SourceText as GHC
import HIndent.Ast.TextValue
import HIndent.Pretty

newtype QuotedText =
  QuotedText TextValue

instance Pretty QuotedText where
  pretty (QuotedText value) = pretty value

mkQuotedText :: GHC.StringLiteral -> QuotedText
mkQuotedText = QuotedText . mkTextValueFromStringLiteral
