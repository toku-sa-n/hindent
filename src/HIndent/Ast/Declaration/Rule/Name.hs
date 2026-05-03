module HIndent.Ast.Declaration.Rule.Name
  ( RuleName
  , mkRuleName
  ) where

import qualified GHC.Data.FastString as GHC
import qualified GHC.Types.Basic as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype RuleName =
  RuleName String
  deriving (Eq, Show)

instance Pretty RuleName where
  pretty (RuleName name) = doubleQuotes $ string name

mkRuleName :: GHC.RuleName -> RuleName
mkRuleName = RuleName . GHC.unpackFS
