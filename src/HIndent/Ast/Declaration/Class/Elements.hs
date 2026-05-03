module HIndent.Ast.Declaration.Class.Elements
  ( ClassElements
  , mkClassElements
  , hasClassElements
  ) where

import Data.Function
import Data.List (sortBy)
import GHC.Types.SrcLoc
import HIndent.Ast.Declaration.Class.Element
import HIndent.Ast.WithComments
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype ClassElements =
  ClassElements [WithComments ClassElement]

instance Pretty ClassElements where
  pretty (ClassElements elements) = newlinePrefixed $ fmap pretty elements

mkClassElements ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LFamilyDecl GhcPs]
  -> [LTyFamDefltDecl GhcPs]
  -> ClassElements
mkClassElements sigs binds families defaults =
  ClassElements
    $ fmap fromGenLocated
    $ sortBy (compare `on` realSrcSpan . locA . getLoc)
    $ fmap (fmap mkClassSignatureElement) sigs
        ++ fmap (fmap mkClassMethodElement) binds
        ++ fmap (fmap mkAssociatedFamilyElement) families
        ++ fmap (fmap mkAssociatedTypeDefaultElement) defaults

hasClassElements :: ClassElements -> Bool
hasClassElements (ClassElements elements) = not $ null elements
