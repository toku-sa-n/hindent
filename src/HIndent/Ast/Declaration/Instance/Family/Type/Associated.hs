{-# LANGUAGE RecordWildCards, CPP #-}

module HIndent.Ast.Declaration.Instance.Family.Type.Associated
  ( AssociatedType
  , mkAssociatedType
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data AssociatedType = AssociatedType
  { name :: WithComments PrefixName
  , types :: [TypeArgument]
  , bind :: WithComments Type
  }

instance Pretty AssociatedType where
  pretty' AssociatedType {..} = do
    spaced $ string "type" : pretty name : fmap pretty types
    string " = "
    pretty bind

mkAssociatedType :: GHC.TyFamInstDecl GHC.GhcPs -> AssociatedType
mkAssociatedType GHC.TyFamInstDecl {GHC.tfid_eqn = GHC.FamEqn {..}} =
  AssociatedType
    { name = fromGenLocated $ fmap mkPrefixName feqn_tycon
    , types = mkTypeArguments feqn_pats
    , bind = mkType <$> fromGenLocated feqn_rhs
    }
