{-# LANGUAGE RecordWildCards, CPP #-}

module HIndent.Ast.Declaration.Instance.Family.Data
  ( DataFamilyInstance
  , mkDataFamilyInstance
  ) where

import qualified GHC.Hs as GG
import HIndent.Ast.Declaration.Data.Body
import HIndent.Ast.Declaration.Data.NewOrData
import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data DataFamilyInstance = DataFamilyInstance
  { newOrData :: NewOrData
  , name :: WithComments PrefixName
  , types :: [TypeArgument]
  , body :: DataBody
  }

instance Pretty DataFamilyInstance where
  pretty DataFamilyInstance {..} = do
    spaced
      $ pretty newOrData : string "instance" : pretty name : fmap pretty types
    pretty body

mkDataFamilyInstance ::
     GHC.FamEqn GHC.GhcPs (GHC.HsDataDefn GHC.GhcPs) -> DataFamilyInstance
mkDataFamilyInstance GHC.FamEqn {..} = DataFamilyInstance {..}
  where
    newOrData = mkNewOrData feqn_rhs
    name = fromGenLocated $ fmap mkPrefixName feqn_tycon
    types = mkTypeArguments feqn_pats
    body = mkDataBody feqn_rhs
