{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module HIndent.HavingComments
  ( HavingComments(..)
  ) where

import           Data.List
import           Generics.SYB
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.PrettyPrint.Pragma

-- | An AST node that implements 'HavingComments' has comments for the
-- node. Note that none of the comments are for the children.
class HavingComments a where
  commentsBefore :: a -> [LEpaComment]
  commentOnSameLine :: a -> Maybe LEpaComment
  commentsAfter :: a -> [LEpaComment]

instance HavingComments HsModule where
  commentsBefore =
    filter (not . isPragma . ac_tok . unLoc) .
    listify (not . isEofComment) . priorComments . comments . hsmodAnn
    where
      isEofComment (L _ (EpaComment EpaEofComment _)) = True
      isEofComment _                                  = False
  commentOnSameLine _ = Nothing
  commentsAfter =
    filter (not . isPragma . ac_tok . unLoc) .
    followingComments . comments . hsmodAnn

instance (HavingComments l) => HavingComments (GenLocated l e) where
  commentsBefore (L l _) = commentsBefore l
  commentOnSameLine (L l _) = commentOnSameLine l
  commentsAfter (L l _) = commentsAfter l

instance HavingComments (HsBind GhcPs) where
  commentsBefore FunBind {..} = commentsBefore fun_id
  commentsBefore _            = []
  commentOnSameLine FunBind {..} = commentOnSameLine fun_id
  commentOnSameLine _            = Nothing
  commentsAfter FunBind {..} = commentsAfter fun_id
  commentsAfter _            = []

instance HavingComments (HsExpr GhcPs) where
  commentsBefore (HsVar _ x)   = commentsBefore x
  commentsBefore (HsApp x _ _) = commentsBefore x
  commentsBefore _             = []
  commentOnSameLine (HsVar _ x)   = commentOnSameLine x
  commentOnSameLine (HsApp x _ _) = commentOnSameLine x
  commentOnSameLine _             = Nothing
  commentsAfter (HsVar _ x)   = commentsAfter x
  commentsAfter (HsApp x _ _) = commentsAfter x
  commentsAfter _             = []

-- | Match GhcPs (LHsExpr GhcPs)
instance HavingComments (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  commentsBefore Match {..} = commentsBefore m_ext
  commentOnSameLine Match {..} = commentOnSameLine m_ext
  commentsAfter Match {..} = commentsAfter m_ext

-- | StmtLR GhcPs GhcPs (LHsExpr GhcPs)
instance HavingComments (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  commentsBefore (LetStmt l _) = commentsBefore l
  commentsBefore _             = []
  commentOnSameLine _ = Nothing
  commentsAfter (LetStmt l _) = commentsAfter l
  commentsAfter _             = []

instance HavingComments (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  commentsBefore (GRHS x _ _) = commentsBefore x
  commentOnSameLine (GRHS x _ _) = commentOnSameLine x
  commentsAfter (GRHS x _ _) = commentsAfter x

instance HavingComments (SrcAnn a) where
  commentsBefore (SrcSpanAnn ep _) = commentsBefore ep
  commentOnSameLine (SrcSpanAnn ep _) = commentOnSameLine ep
  commentsAfter (SrcSpanAnn ep _) = commentsAfter ep

instance HavingComments (EpAnn a) where
  commentsBefore (EpAnn _ _ cs) = priorComments cs
  commentsBefore EpAnnNotUsed   = []
  -- FIXME: Remove duplicated 'where's.
  commentOnSameLine (EpAnn ann _ cs) = find isSameLine $ getFollowingComments cs
    where
      isSameLine (L comAnn _) =
        srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
  commentOnSameLine EpAnnNotUsed = Nothing
  commentsAfter (EpAnn ann _ cs) =
    filter (not . isSameLine) $ getFollowingComments cs
    where
      isSameLine (L comAnn _) =
        srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
  commentsAfter EpAnnNotUsed = []
