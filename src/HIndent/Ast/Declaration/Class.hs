{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Class
  ( ClassDeclaration(..)
  , FunctionalDependency(..)
  , mkClassDeclaration
  ) where

import           HIndent.Ast.Context
import           HIndent.Ast.Declaration.Class.NameAndTypeVariables
import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs                 as GHC
import           HIndent.Pretty.NodeComments

data FunctionalDependency = FunctionalDependency
  { from :: [GHC.LIdP GHC.GhcPs]
  , to   :: [GHC.LIdP GHC.GhcPs]
  }

instance CommentExtraction FunctionalDependency where
  nodeComments FunctionalDependency {} = NodeComments [] [] []

mkFunctionalDependency :: GHC.FunDep GHC.GhcPs -> FunctionalDependency
mkFunctionalDependency (GHC.FunDep _ from to) = FunctionalDependency {..}

data ClassDeclaration = ClassDeclaration
  { context                :: Maybe (WithComments Context)
  , nameAndTypeVariables   :: NameAndTypeVariables
  , functionalDependencies :: [WithComments FunctionalDependency]
  , decl                   :: GHC.TyClDecl GHC.GhcPs
  }

instance CommentExtraction ClassDeclaration where
  nodeComments ClassDeclaration {} = NodeComments [] [] []

mkClassDeclaration :: GHC.TyClDecl GHC.GhcPs -> Maybe ClassDeclaration
mkClassDeclaration x@GHC.ClassDecl {..}
  | Just nameAndTypeVariables <- mkNameAndTypeVariables x =
    Just ClassDeclaration {..}
  where
    context = fmap (fmap mkContext . fromGenLocated) tcdCtxt
    functionalDependencies =
      fmap (fmap mkFunctionalDependency . fromGenLocated) tcdFDs
    decl = x
mkClassDeclaration _ = Nothing
