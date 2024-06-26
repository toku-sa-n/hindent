module HIndent.Ast.Declaration.Warning.Kind
  ( Kind(..)
  ) where

import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Kind
  = Warning
  | Deprecated

instance CommentExtraction Kind where
  nodeComments _ = NodeComments [] [] []

instance Pretty Kind where
  pretty' Warning = string "WARNING"
  pretty' Deprecated = string "DEPRECATED"
