{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Family.Type
  ( TypeFamily
  , mkTypeFamily
  ) where

import Control.Monad
import qualified GHC.Types.Basic as GHC
import HIndent.Applicative
import HIndent.Ast.Declaration.Family.Type.Injectivity
import HIndent.Ast.Declaration.Family.Type.ResultSignature
import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data TypeFamily = TypeFamily
  { isTopLevel :: Bool
  , name :: WithComments PrefixName
  , typeVariables :: [WithComments TypeVariable]
  , signature :: WithComments ResultSignature
  , injectivity :: Maybe (WithComments Injectivity)
  , equations :: Maybe [WithComments TypeEquation]
  }

data TypeEquation = TypeEquation
  { name :: WithComments PrefixName
  , types :: [TypeArgument]
  , bind :: WithComments Type
  }

instance Pretty TypeFamily where
  pretty' TypeFamily {..} = do
    string "type "
    when isTopLevel $ string "family "
    pretty name
    spacePrefixed $ fmap pretty typeVariables
    pretty signature
    whenJust injectivity $ \x -> string " | " >> pretty x
    whenJust equations $ \xs ->
      string " where" >> newline >> indentedBlock (lined $ fmap pretty xs)

instance Pretty TypeEquation where
  pretty' TypeEquation {..} = do
    spaced $ pretty name : fmap pretty types
    string " = "
    pretty bind

mkTypeFamily :: GHC.FamilyDecl GHC.GhcPs -> Maybe TypeFamily
mkTypeFamily GHC.FamilyDecl {fdTyVars = GHC.HsQTvs {..}, ..}
  | GHC.DataFamily <- fdInfo = Nothing
  | otherwise = Just TypeFamily {..}
  where
    isTopLevel =
      case fdTopLevel of
        GHC.TopLevel -> True
        GHC.NotTopLevel -> False
    name = fromGenLocated $ fmap mkPrefixName fdLName
    typeVariables = fmap (fmap mkTypeVariable . fromGenLocated) hsq_explicit
    signature = mkResultSignature <$> fromGenLocated fdResultSig
    injectivity = fmap (fmap mkInjectivity . fromGenLocated) fdInjectivityAnn
    equations =
      case fdInfo of
        GHC.DataFamily -> error "Not a TypeFamily"
        GHC.OpenTypeFamily -> Nothing
        GHC.ClosedTypeFamily Nothing -> Just []
        GHC.ClosedTypeFamily (Just xs) ->
          Just $ fmap (fmap mkTypeEquation . fromGenLocated) xs

mkTypeEquation :: GHC.TyFamInstEqn GHC.GhcPs -> TypeEquation
mkTypeEquation GHC.FamEqn {..} =
  TypeEquation
    { name = fromGenLocated $ fmap mkPrefixName feqn_tycon
    , types = mkTypeArguments feqn_pats
    , bind = mkType <$> fromGenLocated feqn_rhs
    }
