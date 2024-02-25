{-# LANGUAGE RecordWildCards #-}

-- | AST with comments.
module HIndent.Ast.WithComments
  ( WithComments
  , mkWithComments
  , mkWithCommentsWithEpAnn
  , mkWithCommentsWithSrcAnn
  , mkWithCommentsWithGenLocated
  ) where

import GHC.Hs
import GHC.Types.SrcLoc
import HIndent.Ast.Pragma
import HIndent.Pretty
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

data WithComments a = WithComments
  { comments :: NodeComments
  , node :: a
  }

instance Functor WithComments where
  fmap f (WithComments c n) = WithComments c (f n)

instance CommentExtraction (WithComments a) where
  nodeComments (WithComments c _) = c

instance (Pretty a) => Pretty (WithComments a) where
  pretty' (WithComments {..}) = pretty' node

-- TODO: Remove this function.
mkWithComments :: a -> WithComments a
mkWithComments = WithComments (NodeComments [] [] [])

mkWithCommentsWithEpAnn :: EpAnn a -> b -> WithComments b
mkWithCommentsWithEpAnn ann =
  WithComments (epaComments $ filterOutEofAndPragmasFromAnn ann)

mkWithCommentsWithSrcAnn :: SrcAnn a -> b -> WithComments b
mkWithCommentsWithSrcAnn SrcSpanAnn {..} = mkWithCommentsWithEpAnn ann

mkWithCommentsWithGenLocated :: GenLocated (SrcAnn a) b -> WithComments b
mkWithCommentsWithGenLocated (L ann x) = mkWithCommentsWithSrcAnn ann x

epaComments :: EpAnn a -> NodeComments
epaComments (EpAnn ann _ cs) = NodeComments {..}
  where
    commentsBefore = priorComments cs
    commentsOnSameLine = filter isCommentOnSameLine $ getFollowingComments cs
    commentsAfter = filter (not . isCommentOnSameLine) $ getFollowingComments cs
    isCommentOnSameLine (L comAnn _) =
      srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
epaComments EpAnnNotUsed = NodeComments [] [] []

filterOutEofAndPragmasFromAnn :: EpAnn ann -> EpAnn ann
filterOutEofAndPragmasFromAnn EpAnn {..} =
  EpAnn {comments = filterOutEofAndPragmasFromComments comments, ..}
filterOutEofAndPragmasFromAnn EpAnnNotUsed = EpAnnNotUsed

filterOutEofAndPragmasFromComments :: EpAnnComments -> EpAnnComments
filterOutEofAndPragmasFromComments comments =
  EpaCommentsBalanced
    { priorComments = filterOutEofAndPragmas $ priorComments comments
    , followingComments = filterOutEofAndPragmas $ getFollowingComments comments
    }

filterOutEofAndPragmas :: [GenLocated l EpaComment] -> [GenLocated l EpaComment]
filterOutEofAndPragmas = filter isNeitherEofNorPragmaComment

isNeitherEofNorPragmaComment :: GenLocated l EpaComment -> Bool
isNeitherEofNorPragmaComment (L _ (EpaComment EpaEofComment _)) = False
isNeitherEofNorPragmaComment (L _ (EpaComment tok _)) = not $ isPragma tok
