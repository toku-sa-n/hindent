{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.WhereClause
  ( WhereClause
  , mkWhereClause
  , mkMatchWhereClause
  , mkPatternWhereClause
  ) where

import HIndent.Ast.LocalBinds (LocalBinds, mkLocalBinds)
import qualified HIndent.Ast.NodeComments as NodeComments
import HIndent.Ast.WithComments
  ( WithComments
  , addComments
  , mkWithComments
  , prettyWith
  )
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype WhereClause = WhereClause
  { binds :: WithComments LocalBinds
  }

instance Pretty WhereClause where
  pretty WhereClause {..} =
    lined [string "where", prettyWith binds $ indentedBlock . pretty]

mkWhereClause :: GHC.GRHSs GHC.GhcPs body -> Maybe (WithComments WhereClause)
mkWhereClause GHC.GRHSs {..} = do
  binds <- mkLocalBinds grhssLocalBinds
  pure
    $ addComments (NodeComments.fromEpAnnComments grhssExt)
    $ mkWithComments WhereClause {binds = binds}

mkMatchWhereClause ::
     GHC.Match GHC.GhcPs body -> Maybe (WithComments WhereClause)
mkMatchWhereClause GHC.Match {..} =
  addComments (NodeComments.fromAnnotation m_ext) <$> mkWhereClause m_grhss

mkPatternWhereClause :: GHC.HsBind GHC.GhcPs -> Maybe (WithComments WhereClause)
mkPatternWhereClause GHC.PatBind {..} =
  addComments (NodeComments.fromAnnotation pat_ext) <$> mkWhereClause pat_rhs
mkPatternWhereClause _ = error "This AST node should not appear."
