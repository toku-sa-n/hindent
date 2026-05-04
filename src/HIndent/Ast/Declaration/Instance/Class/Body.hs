module HIndent.Ast.Declaration.Instance.Class.Body
  ( ClassInstanceBody
  , hasClassInstanceBody
  , mkClassInstanceBody
  ) where

import Data.Function
import Data.List (sortBy)
import GHC.Types.SrcLoc
import HIndent.Ast.Declaration.Instance.Class.Member
import HIndent.Ast.WithComments
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype ClassInstanceBody =
  ClassInstanceBody [WithComments ClassInstanceMember]

instance Pretty ClassInstanceBody where
  pretty (ClassInstanceBody members) = lined $ fmap pretty members

mkClassInstanceBody ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LTyFamInstDecl GhcPs]
  -> [LDataFamInstDecl GhcPs]
  -> ClassInstanceBody
mkClassInstanceBody sigs binds typeInstances dataInstances =
  ClassInstanceBody
    $ fmap fromGenLocated
    $ sortBy (compare `on` realSrcSpan . locA . getLoc)
    $ fmap (fmap mkClassInstanceSignatureMember) sigs
        ++ fmap (fmap mkClassInstanceMethodMember) binds
        ++ fmap (fmap mkAssociatedTypeInstanceMember) typeInstances
        ++ fmap (fmap mkAssociatedDataInstanceMember) dataInstances

hasClassInstanceBody :: ClassInstanceBody -> Bool
hasClassInstanceBody (ClassInstanceBody members) = not $ null members
