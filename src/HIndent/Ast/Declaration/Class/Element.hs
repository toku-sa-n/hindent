module HIndent.Ast.Declaration.Class.Element
  ( ClassElement
  , mkClassSignatureElement
  , mkClassMethodElement
  , mkAssociatedFamilyElement
  , mkAssociatedTypeDefaultElement
  ) where

import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Family.Data
import HIndent.Ast.Declaration.Family.Type
import HIndent.Ast.Declaration.Instance.Family.Type.Associated.Default
import HIndent.Ast.Declaration.Signature
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty

data ClassElement
  = Signature Signature
  | Method Bind
  | AssociatedDataFamily DataFamily
  | AssociatedTypeDefault AssociatedTypeDefault
  | AssociatedTypeFamily TypeFamily

instance Pretty ClassElement where
  pretty (Signature signature) = pretty signature
  pretty (Method bind) = pretty bind
  pretty (AssociatedDataFamily dataFamily) = pretty dataFamily
  pretty (AssociatedTypeDefault associatedTypeDefault) =
    pretty associatedTypeDefault
  pretty (AssociatedTypeFamily typeFamily) = pretty typeFamily

mkClassSignatureElement :: GHC.Sig GHC.GhcPs -> ClassElement
mkClassSignatureElement = Signature . mkSignature

mkClassMethodElement :: GHC.HsBind GHC.GhcPs -> ClassElement
mkClassMethodElement = Method . mkBind

mkAssociatedFamilyElement :: GHC.FamilyDecl GHC.GhcPs -> ClassElement
mkAssociatedFamilyElement familyDecl
  | Just typeFamily <- mkTypeFamily familyDecl = AssociatedTypeFamily typeFamily
  | Just dataFamily <- mkDataFamily familyDecl = AssociatedDataFamily dataFamily
  | otherwise = error "Unreachable"

mkAssociatedTypeDefaultElement :: GHC.TyFamInstDecl GHC.GhcPs -> ClassElement
mkAssociatedTypeDefaultElement = AssociatedTypeDefault . mkAssociatedTypeDefault
