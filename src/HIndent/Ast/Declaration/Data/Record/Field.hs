{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Record.Field
  ( RecordField
  , mkRecordField
  ) where

import HIndent.Ast.Declaration.Data.Record.FieldName
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RecordField = RecordField
  { names :: [WithComments FieldName]
  , ty :: GHC.LBangType GHC.GhcPs
  }

instance CommentExtraction RecordField where
  nodeComments RecordField {} = NodeComments [] [] []

instance Pretty RecordField where
  pretty' RecordField {..} =
    spaced [hCommaSep $ fmap pretty names, string "::", pretty $ fmap mkType ty]

mkRecordField :: GHC.ConDeclField GHC.GhcPs -> RecordField
mkRecordField GHC.ConDeclField {..} = RecordField {..}
  where
    names = fmap mkFieldName . fromGenLocated <$> cd_fld_names
    ty = cd_fld_type
