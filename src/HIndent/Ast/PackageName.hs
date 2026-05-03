module HIndent.Ast.PackageName
  ( PackageName
  , mkPackageName
  ) where

import qualified GHC.Types.SourceText as GHC
import HIndent.Ast.TextValue
import HIndent.Pretty

newtype PackageName =
  PackageName TextValue

instance Pretty PackageName where
  pretty (PackageName value) = pretty value

mkPackageName :: GHC.StringLiteral -> PackageName
mkPackageName = PackageName . mkTextValueFromStringLiteral
