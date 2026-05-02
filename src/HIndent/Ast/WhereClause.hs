{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.WhereClause
  ( WhereClause
  , mkWhereClause
  , mkMatchWhereClause
  , mkPatternWhereClause
  , prettyWhereClause
  ) where

import HIndent.Ast.LocalBinds (LocalBinds, mkLocalBinds)
import qualified HIndent.Ast.NodeComments as NodeComments
import HIndent.Ast.WithComments
  ( WithComments
  , addComments
  , getComments
  , getNode
  , mkWithComments
  , prettyWith
  )
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Printer

newtype WhereClause = WhereClause
  { binds :: WithComments LocalBinds
  }

instance Pretty WhereClause where
  pretty' WhereClause {..} =
    lined [string "where", prettyWith binds $ indentedBlock . pretty]

mkWhereClause :: GHC.GRHSs GHC.GhcPs body -> Maybe (WithComments WhereClause)
mkWhereClause GHC.GRHSs {..} =
  mkWhereClause' (NodeComments.fromEpAnnComments grhssExt) grhssLocalBinds

mkMatchWhereClause ::
     GHC.Match GHC.GhcPs body -> Maybe (WithComments WhereClause)
mkMatchWhereClause GHC.Match {..} =
  addComments (NodeComments.fromAnnotation m_ext) <$> mkWhereClause m_grhss

mkPatternWhereClause :: GHC.HsBind GHC.GhcPs -> Maybe (WithComments WhereClause)
mkPatternWhereClause GHC.PatBind {..} =
  addComments (NodeComments.fromAnnotation pat_ext) <$> mkWhereClause pat_rhs
mkPatternWhereClause _ = error "This AST node should not appear."

prettyWhereClause :: WithComments WhereClause -> Printer ()
prettyWhereClause whereClause =
  indentedBlock
    $ newlinePrefixed
    $ precedingComments whereClause <> [pretty $ getNode whereClause]

precedingComments :: WithComments WhereClause -> [Printer ()]
precedingComments whereClause
  | getComments whereClause == mempty = []
  | otherwise =
    [ prettyWith
        (addComments (getComments whereClause) $ mkWithComments ())
        (const $ pure ())
    ]

mkWhereClause' ::
     NodeComments.NodeComments
  -> GHC.HsLocalBinds GHC.GhcPs
  -> Maybe (WithComments WhereClause)
mkWhereClause' comments localBinds = do
  binds <- mkLocalBinds localBinds
  pure $ addComments comments $ mkWithComments WhereClause {binds = binds}
