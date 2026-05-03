module HIndent.Ast.LocalBinds.Element
  ( LocalBindElement
  , mkLocalBindSignatureElement
  , mkLocalBindingElement
  ) where

import {-# SOURCE #-} HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Signature
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty

data LocalBindElement
  = Binding Bind
  | Signature Signature

instance Pretty LocalBindElement where
  pretty (Binding bind) = pretty bind
  pretty (Signature signature) = pretty signature

mkLocalBindSignatureElement :: GHC.Sig GHC.GhcPs -> LocalBindElement
mkLocalBindSignatureElement = Signature . mkSignature

mkLocalBindingElement :: GHC.HsBind GHC.GhcPs -> LocalBindElement
mkLocalBindingElement = Binding . mkBind
