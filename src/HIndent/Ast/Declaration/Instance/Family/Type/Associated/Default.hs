{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Instance.Family.Type.Associated.Default
  ( AssociatedTypeDefault
  , mkAssociatedTypeDefault
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data AssociatedTypeDefault = AssociatedTypeDefault
  { name :: WithComments PrefixName
  , types :: [TypeArgument]
  , bind :: WithComments Type
  }

instance Pretty AssociatedTypeDefault where
  pretty AssociatedTypeDefault {..} = do
    spaced $ string "type instance" : pretty name : fmap pretty types
    string " = "
    pretty bind

mkAssociatedTypeDefault :: GHC.TyFamInstDecl GHC.GhcPs -> AssociatedTypeDefault
mkAssociatedTypeDefault GHC.TyFamInstDecl {GHC.tfid_eqn = GHC.FamEqn {..}} =
  AssociatedTypeDefault
    { name = fromGenLocated $ fmap mkPrefixName feqn_tycon
    , types = mkTypeArguments feqn_pats
    , bind = mkType <$> fromGenLocated feqn_rhs
    }
