{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family.Type.Equation
  ( TypeEquation
  , mkTypeEquation
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data TypeEquation = TypeEquation
  { name :: WithComments PrefixName
  , types :: [TypeArgument]
  , bind :: WithComments Type
  }

instance Pretty TypeEquation where
  pretty TypeEquation {..} = do
    spaced $ pretty name : fmap pretty types
    string " = "
    pretty bind

mkTypeEquation :: GHC.TyFamInstEqn GHC.GhcPs -> TypeEquation
mkTypeEquation GHC.FamEqn {..} =
  TypeEquation
    { name = fromGenLocated $ fmap mkPrefixName feqn_tycon
    , types = mkTypeArguments feqn_pats
    , bind = mkType <$> fromGenLocated feqn_rhs
    }
