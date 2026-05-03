{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Family.Data.Associated
  ( AssociatedDataFamilyInstance
  , mkAssociatedDataFamilyInstance
  ) where

import HIndent.Ast.Declaration.Data.Body
import HIndent.Ast.Declaration.Data.NewOrData
import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data AssociatedDataFamilyInstance = AssociatedDataFamilyInstance
  { newOrData :: NewOrData
  , name :: WithComments PrefixName
  , types :: [TypeArgument]
  , body :: DataBody
  }

instance Pretty AssociatedDataFamilyInstance where
  pretty AssociatedDataFamilyInstance {..} = do
    spaced $ pretty newOrData : pretty name : fmap pretty types
    pretty body

mkAssociatedDataFamilyInstance ::
     GHC.DataFamInstDecl GHC.GhcPs -> AssociatedDataFamilyInstance
mkAssociatedDataFamilyInstance
  GHC.DataFamInstDecl {GHC.dfid_eqn = GHC.FamEqn {..}} =
    AssociatedDataFamilyInstance
      { newOrData = mkNewOrData feqn_rhs
      , name = fromGenLocated $ fmap mkPrefixName feqn_tycon
      , types = mkTypeArguments feqn_pats
      , body = mkDataBody feqn_rhs
      }
