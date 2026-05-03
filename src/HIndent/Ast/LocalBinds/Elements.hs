module HIndent.Ast.LocalBinds.Elements
  ( LocalBindElements
  , hasLocalBindElements
  , mkLocalBindElements
  ) where

import Data.Function
import Data.List (sortBy)
import GHC.Types.SrcLoc
import HIndent.Ast.LocalBinds.Element
import HIndent.Ast.WithComments
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype LocalBindElements =
  LocalBindElements [WithComments LocalBindElement]

instance Pretty LocalBindElements where
  pretty (LocalBindElements elements) = lined $ fmap pretty elements

mkLocalBindElements ::
     [LSig GhcPs] -> [LHsBindLR GhcPs GhcPs] -> LocalBindElements
mkLocalBindElements sigs binds =
  LocalBindElements
    $ fmap fromGenLocated
    $ sortBy (compare `on` realSrcSpan . locA . getLoc)
    $ fmap (fmap mkLocalBindSignatureElement) sigs
        ++ fmap (fmap mkLocalBindingElement) binds

hasLocalBindElements :: LocalBindElements -> Bool
hasLocalBindElements (LocalBindElements elements) = not $ null elements
