module HIndent.Ast.Type.Literal
  ( Literal
  , mkLiteral
  ) where

import qualified Data.Text as Text
import qualified GHC.Data.FastString as GHC
import HIndent.Ast.TextValue
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import Text.Show.Unicode

data Literal
  = Numeric Integer
  | String TextValue
  | Character Char

instance Pretty Literal where
  pretty (Numeric n) = string $ Text.pack $ show n
  pretty (String s) = string $ Text.pack $ ushow $ toText s
  pretty (Character c) = string $ Text.pack $ ushow c

mkLiteral :: GHC.HsTyLit GHC.GhcPs -> Literal
mkLiteral (GHC.HsNumTy _ n) = Numeric n
mkLiteral (GHC.HsStrTy _ s) = String $ mkTextValueFromString $ GHC.unpackFS s
mkLiteral (GHC.HsCharTy _ c) = Character c
