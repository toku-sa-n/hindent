module HIndent.Ast.Declaration.Instance.Class.Elements
  ( ClassInstanceElements
  , hasClassInstanceElements
  , mkClassInstanceElements
  ) where

import Data.Function
import Data.List (sortBy)
import GHC.Types.SrcLoc
import HIndent.Ast.Declaration.Instance.Class.Element
import HIndent.Ast.WithComments
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype ClassInstanceElements =
  ClassInstanceElements [WithComments ClassInstanceElement]

instance Pretty ClassInstanceElements where
  pretty (ClassInstanceElements elements) = lined $ fmap pretty elements

mkClassInstanceElements ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LTyFamInstDecl GhcPs]
  -> [LDataFamInstDecl GhcPs]
  -> ClassInstanceElements
mkClassInstanceElements sigs binds typeInstances dataInstances =
  ClassInstanceElements
    $ fmap fromGenLocated
    $ sortBy (compare `on` realSrcSpan . locA . getLoc)
    $ fmap (fmap mkClassInstanceSignatureElement) sigs
        ++ fmap (fmap mkClassInstanceMethodElement) binds
        ++ fmap (fmap mkAssociatedTypeInstanceElement) typeInstances
        ++ fmap (fmap mkAssociatedDataInstanceElement) dataInstances

hasClassInstanceElements :: ClassInstanceElements -> Bool
hasClassInstanceElements (ClassInstanceElements elements) = not $ null elements
