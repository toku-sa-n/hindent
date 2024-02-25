{-# LANGUAGE RecordWildCards #-}

-- | AST with comments.
module HIndent.Ast.WithComments
  ( WithComments
  , mkWithComments
  , mkWithCommentsWithEpAnn
  ) where

import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types

data WithComments a = WithComments
  { comments :: NodeComments
  , node     :: a
  }

instance CommentExtraction (WithComments a) where
  nodeComments (WithComments c _) = c

instance (Pretty a) => Pretty (WithComments a) where
  pretty' (WithComments {..}) = pretty' node

-- TODO: Remove this function.
mkWithComments :: a -> WithComments a
mkWithComments = WithComments (NodeComments [] [] [])

mkWithCommentsWithEpAnn :: EpAnn a -> b -> WithComments b
mkWithCommentsWithEpAnn ann = WithComments (epaComments ann)

epaComments :: EpAnn a -> NodeComments
epaComments (EpAnn ann _ cs) = NodeComments {..}
  where
    commentsBefore = priorComments cs
    commentsOnSameLine = filter isCommentOnSameLine $ getFollowingComments cs
    commentsAfter = filter (not . isCommentOnSameLine) $ getFollowingComments cs
    isCommentOnSameLine (L comAnn _) =
      srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
epaComments EpAnnNotUsed = NodeComments [] [] []
