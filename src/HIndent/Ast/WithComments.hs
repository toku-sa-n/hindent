-- | AST with comments.
module HIndent.Ast.WithComments
  ( WithComments(..)
  ) where

import           HIndent.Pretty.Types

data WithComments a = WithComments
  { comments :: NodeComments
  , node     :: a
  }
