module HIndent.Ast.Declaration.Class.Body
  ( ClassBody
  , mkClassBody
  , hasClassBody
  ) where

import Data.Function
import Data.List (sortBy)
import GHC.Types.SrcLoc
import HIndent.Ast.Declaration.Class.Member
import HIndent.Ast.WithComments
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype ClassBody =
  ClassBody [WithComments ClassMember]

instance Pretty ClassBody where
  pretty (ClassBody members) = newlinePrefixed $ fmap pretty members

mkClassBody ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LFamilyDecl GhcPs]
  -> [LTyFamDefltDecl GhcPs]
  -> ClassBody
mkClassBody sigs binds families defaults =
  ClassBody
    $ fmap fromGenLocated
    $ sortBy (compare `on` realSrcSpan . locA . getLoc)
    $ fmap (fmap mkClassSignatureMember) sigs
        ++ fmap (fmap mkClassMethodMember) binds
        ++ fmap (fmap mkAssociatedFamilyMember) families
        ++ fmap (fmap mkAssociatedTypeDefaultMember) defaults

hasClassBody :: ClassBody -> Bool
hasClassBody (ClassBody members) = not $ null members
