module HIndent.Ast.Module.Name
  ( ModuleName
  , mkModuleName
  ) where

import qualified GHC.Unit as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype ModuleName =
  ModuleName String
  deriving (Eq, Ord)

instance Pretty ModuleName where
  pretty (ModuleName x) = string x

mkModuleName :: GHC.ModuleName -> ModuleName
mkModuleName = ModuleName . GHC.moduleNameString
