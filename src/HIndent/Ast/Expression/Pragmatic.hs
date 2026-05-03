{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.Pragmatic
  ( ExpressionPragma
  , mkExpressionPragma
  ) where

import qualified GHC.Hs as GHC
import qualified HIndent.Ast.NodeComments as NodeComments
import HIndent.Ast.StringLiteral
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
import HIndent.Ast.WithComments (WithComments, addComments, mkWithComments)
#else
import HIndent.Ast.WithComments
  ( WithComments
  , addComments
  , fromEpAnn
  , mkWithComments
  )
#endif
import HIndent.Pretty (Pretty(..))
import HIndent.Pretty.Combinators (spaced, string)

newtype ExpressionPragma = SccPragma
  { label :: StringLiteral
  }

instance Pretty ExpressionPragma where
  pretty SccPragma {..} = spaced [string "{-# SCC", pretty label, string "#-}"]

mkExpressionPragma :: GHC.HsPragE GHC.GhcPs -> WithComments ExpressionPragma
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
mkExpressionPragma (GHC.HsPragSCC (ann, _) literal) =
  addComments (NodeComments.fromAnnotation ann)
    $ mkWithComments SccPragma {label = mkStringLiteral literal}
#else
mkExpressionPragma (GHC.HsPragSCC ann _ literal) =
  fromEpAnn ann SccPragma {label = mkStringLiteral literal}
#endif
