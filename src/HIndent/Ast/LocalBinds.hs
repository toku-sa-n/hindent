{-# LANGUAGE CPP #-}

module HIndent.Ast.LocalBinds
  ( LocalBinds
  , mkLocalBinds
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
import HIndent.Ast.LocalBinds.Elements
import HIndent.Ast.LocalBinds.ImplicitBindings
  ( ImplicitBindings
  , mkImplicitBindings
  )
import HIndent.Ast.WithComments (WithComments, fromEpAnn)
import HIndent.Pretty (Pretty(..))

data LocalBinds
  = Value
      { localElements :: LocalBindElements
      }
  | ImplicitParameters
      { implicitBindings :: ImplicitBindings
      }

instance Pretty LocalBinds where
  pretty Value {localElements = elements} = pretty elements
  pretty (ImplicitParameters {implicitBindings = binds}) = pretty binds

mkLocalBinds :: GHC.HsLocalBinds GHC.GhcPs -> Maybe (WithComments LocalBinds)
mkLocalBinds (GHC.HsValBinds ann binds) =
  Just $ fromEpAnn ann $ Value {localElements = mkLocalElements binds}
mkLocalBinds (GHC.HsIPBinds ann binds) =
  Just
    $ fromEpAnn ann
    $ ImplicitParameters {implicitBindings = mkImplicitBindings binds}
mkLocalBinds GHC.EmptyLocalBinds {} = Nothing

mkLocalElements :: GHC.HsValBindsLR GHC.GhcPs GHC.GhcPs -> LocalBindElements
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkLocalElements (GHC.ValBinds _ binds sigs) = mkLocalBindElements sigs binds
#else
mkLocalElements (GHC.ValBinds _ bindBag sigs) =
  mkLocalBindElements sigs (GHC.bagToList bindBag)
#endif
mkLocalElements GHC.XValBindsLR {} =
  error "`ghc-lib-parser` never generates this AST node."
