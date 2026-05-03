module HIndent.Ast.Declaration.Instance.Class.Element
  ( ClassInstanceElement
  , mkAssociatedDataInstanceElement
  , mkAssociatedTypeInstanceElement
  , mkClassInstanceMethodElement
  , mkClassInstanceSignatureElement
  ) where

import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Instance.Family.Data.Associated
import HIndent.Ast.Declaration.Instance.Family.Type.Associated
import HIndent.Ast.Declaration.Signature
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty

data ClassInstanceElement
  = AssociatedDataInstance AssociatedDataFamilyInstance
  | AssociatedTypeInstance AssociatedType
  | Method Bind
  | Signature Signature

instance Pretty ClassInstanceElement where
  pretty (AssociatedDataInstance associatedDataInstance) =
    pretty associatedDataInstance
  pretty (AssociatedTypeInstance associatedTypeInstance) =
    pretty associatedTypeInstance
  pretty (Method bind) = pretty bind
  pretty (Signature signature) = pretty signature

mkAssociatedDataInstanceElement ::
     GHC.DataFamInstDecl GHC.GhcPs -> ClassInstanceElement
mkAssociatedDataInstanceElement =
  AssociatedDataInstance . mkAssociatedDataFamilyInstance

mkAssociatedTypeInstanceElement ::
     GHC.TyFamInstDecl GHC.GhcPs -> ClassInstanceElement
mkAssociatedTypeInstanceElement = AssociatedTypeInstance . mkAssociatedType

mkClassInstanceMethodElement :: GHC.HsBind GHC.GhcPs -> ClassInstanceElement
mkClassInstanceMethodElement = Method . mkBind

mkClassInstanceSignatureElement :: GHC.Sig GHC.GhcPs -> ClassInstanceElement
mkClassInstanceSignatureElement = Signature . mkSignature
