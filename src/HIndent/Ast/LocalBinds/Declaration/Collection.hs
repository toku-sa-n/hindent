module HIndent.Ast.LocalBinds.Declaration.Collection
  ( LocalDeclarationCollection
  , hasLocalDeclarations
  , mkLocalDeclarationCollection
  ) where

import Data.Function
import Data.List (sortBy)
import GHC.Types.SrcLoc
import HIndent.Ast.LocalBinds.Declaration
import HIndent.Ast.WithComments
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype LocalDeclarationCollection =
  LocalDeclarationCollection [WithComments LocalDeclaration]

instance Pretty LocalDeclarationCollection where
  pretty (LocalDeclarationCollection declarations) =
    lined $ fmap pretty declarations

mkLocalDeclarationCollection ::
     [LSig GhcPs] -> [LHsBindLR GhcPs GhcPs] -> LocalDeclarationCollection
mkLocalDeclarationCollection sigs binds =
  LocalDeclarationCollection
    $ fmap fromGenLocated
    $ sortBy (compare `on` realSrcSpan . locA . getLoc)
    $ fmap (fmap mkLocalSignatureDeclaration) sigs
        ++ fmap (fmap mkLocalBindingDeclaration) binds

hasLocalDeclarations :: LocalDeclarationCollection -> Bool
hasLocalDeclarations (LocalDeclarationCollection declarations) =
  not $ null declarations
