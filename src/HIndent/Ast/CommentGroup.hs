{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module HIndent.Ast.CommentGroup
  ( CommentGroup(..)
  , HasComments
  , mkCommentGroup
  , mkCommentGroupFromEpAnn
  , mkCommentGroupFromEpAnnComments
  , mkCommentGroupFromEpaLocation
  ) where

import Control.Applicative ((<|>))
import Data.Data (Data, cast, gmapQl)
import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Comment (Comment, mkComment)
import qualified HIndent.GhcLibParserWrapper.GHC.Parser.Annotation as Annotation
import HIndent.Pragma

data CommentGroup = CommentGroup
  { commentsBefore :: [Comment]
  , commentsOnSameLine :: [Comment]
  , commentsAfter :: [Comment]
  } deriving (Eq)
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
type CommentEntryLocation = GHC.EpaLocation
#else
type CommentEntryLocation = Annotation.Anchor
#endif
data CommentSource
  = forall a. FromEpAnn (GHC.EpAnn a)
  | FromEntryAndComments CommentEntryLocation GHC.EpAnnComments
  | FromEpAnnComments GHC.EpAnnComments
  | FromEntryLocation CommentEntryLocation
  | NoComments

class HasComments a where
  getCommentSource :: a -> CommentSource

instance Semigroup CommentGroup where
  x <> y =
    CommentGroup
      { commentsBefore = commentsBefore x <> commentsBefore y
      , commentsOnSameLine = commentsOnSameLine x <> commentsOnSameLine y
      , commentsAfter = commentsAfter x <> commentsAfter y
      }

instance Monoid CommentGroup where
  mempty =
    CommentGroup
      {commentsBefore = [], commentsOnSameLine = [], commentsAfter = []}

instance HasComments GHC.EpAnnComments where
  getCommentSource = FromEpAnnComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance HasComments GHC.EpaLocation where
  getCommentSource = FromEntryLocation
#else
instance HasComments Annotation.Anchor where
  getCommentSource = FromEntryLocation
#endif
instance HasComments GHC.EpAnnCO where
  getCommentSource = FromEpAnn

instance HasComments GHC.NoEpAnns where
  getCommentSource = const NoComments

instance HasComments GHC.SrcSpan where
  getCommentSource = const NoComments

instance HasComments Annotation.SrcSpanAnnA where
  getCommentSource = FromEpAnn . Annotation.srcSpanAnnAToEpAnn

instance HasComments Annotation.SrcSpanAnnL where
  getCommentSource = FromEpAnn . Annotation.srcSpanAnnLToEpAnn

instance HasComments Annotation.SrcSpanAnnN where
  getCommentSource = FromEpAnn . Annotation.srcSpanAnnNToEpAnn

instance HasComments Annotation.SrcSpanAnnP where
  getCommentSource = FromEpAnn . Annotation.srcSpanAnnPToEpAnn

instance HasComments Annotation.SrcSpanAnnC where
  getCommentSource = FromEpAnn . Annotation.srcSpanAnnCToEpAnn

instance {-# OVERLAPPABLE #-} Data a => HasComments a where
  getCommentSource = mkCommentSourceFromData

mkCommentGroup :: HasComments a => a -> CommentGroup
mkCommentGroup = mkCommentGroupFromCommentSource . getCommentSource

mkCommentGroupFromEpAnn :: GHC.EpAnn a -> CommentGroup
mkCommentGroupFromEpAnn =
  mkCommentGroupFromEpAnn' . filterOutEofAndPragmasFromAnn

mkCommentGroupFromEpAnnComments :: GHC.EpAnnComments -> CommentGroup
mkCommentGroupFromEpAnnComments comments =
  CommentGroup
    { commentsBefore = mkComment <$> GHC.priorComments filteredComments
    , commentsOnSameLine = []
    , commentsAfter = mkComment <$> GHC.getFollowingComments filteredComments
    }
  where
    filteredComments = filterOutEofAndPragmasFromComments comments

mkCommentGroupFromEntryAndComments ::
     CommentEntryLocation -> GHC.EpAnnComments -> CommentGroup
mkCommentGroupFromEntryAndComments entry comments =
  mkCommentGroupFromCommentEntry entry <> CommentGroup {..}
  where
    filteredComments = filterOutEofAndPragmasFromComments comments
    commentsBefore = mkComment <$> GHC.priorComments filteredComments
    commentsOnSameLine =
      fmap mkComment
        $ filter isCommentOnSameLine
        $ GHC.getFollowingComments filteredComments
    commentsAfter =
      fmap mkComment
        $ filter (not . isCommentOnSameLine)
        $ GHC.getFollowingComments filteredComments
    isCommentOnSameLine (GHC.L commentLoc _) =
      GHC.srcSpanEndLine (Annotation.epaLocationToRealSrcSpan entry)
        == GHC.srcSpanStartLine (Annotation.epaLocationToRealSrcSpan commentLoc)

mkCommentGroupFromEpaLocation :: GHC.EpaLocation -> CommentGroup
mkCommentGroupFromEpaLocation GHC.EpaSpan {} = mempty
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCommentGroupFromEpaLocation (GHC.EpaDelta _ _ trailing) =
  foldMap mkCommentGroupFromTrailingComment trailing
#else
mkCommentGroupFromEpaLocation (GHC.EpaDelta _ trailing) =
  foldMap mkCommentGroupFromTrailingComment trailing
#endif
mkCommentGroupFromCommentSource :: CommentSource -> CommentGroup
mkCommentGroupFromCommentSource (FromEpAnn ann) = mkCommentGroupFromEpAnn ann
mkCommentGroupFromCommentSource (FromEntryAndComments entry comments) =
  mkCommentGroupFromEntryAndComments entry comments
mkCommentGroupFromCommentSource (FromEpAnnComments comments) =
  mkCommentGroupFromEpAnnComments comments
mkCommentGroupFromCommentSource (FromEntryLocation location) =
  mkCommentGroupFromCommentEntry location
mkCommentGroupFromCommentSource NoComments = mempty

mkCommentGroupFromEpAnn' :: GHC.EpAnn a -> CommentGroup
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCommentGroupFromEpAnn' GHC.EpAnn {..} =
  mkCommentGroupFromEpAnnEntry entry <> CommentGroup {..}
  where
    commentsBefore = mkComment <$> GHC.priorComments comments
    commentsOnSameLine =
      fmap mkComment
        $ filter isCommentOnSameLine
        $ GHC.getFollowingComments comments
    commentsAfter =
      fmap mkComment
        $ filter (not . isCommentOnSameLine)
        $ GHC.getFollowingComments comments
    isCommentOnSameLine (GHC.L commentLoc _) =
      GHC.srcSpanEndLine (Annotation.epaLocationToRealSrcSpan entry)
        == GHC.srcSpanStartLine (Annotation.epaLocationToRealSrcSpan commentLoc)
#else
mkCommentGroupFromEpAnn' GHC.EpAnn {..} =
  mkCommentGroupFromEpAnnEntry entry <> CommentGroup {..}
  where
    commentsBefore = mkComment <$> GHC.priorComments comments
    commentsOnSameLine =
      fmap mkComment
        $ filter isCommentOnSameLine
        $ GHC.getFollowingComments comments
    commentsAfter =
      fmap mkComment
        $ filter (not . isCommentOnSameLine)
        $ GHC.getFollowingComments comments
    isCommentOnSameLine (GHC.L commentLoc _) =
      GHC.srcSpanEndLine (Annotation.epaLocationToRealSrcSpan entry)
        == GHC.srcSpanStartLine (Annotation.epaLocationToRealSrcSpan commentLoc)
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCommentGroupFromEpAnn' GHC.EpAnnNotUsed = mempty
#endif
#endif
filterOutEofAndPragmasFromAnn :: GHC.EpAnn ann -> GHC.EpAnn ann
filterOutEofAndPragmasFromAnn GHC.EpAnn {..} =
  GHC.EpAnn {comments = filterOutEofAndPragmasFromComments comments, ..}
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
filterOutEofAndPragmasFromAnn GHC.EpAnnNotUsed = GHC.EpAnnNotUsed
#endif
filterOutEofAndPragmasFromComments :: GHC.EpAnnComments -> GHC.EpAnnComments
filterOutEofAndPragmasFromComments comments =
  GHC.EpaCommentsBalanced
    { priorComments = filterOutEofAndPragmas $ GHC.priorComments comments
    , followingComments =
        filterOutEofAndPragmas $ GHC.getFollowingComments comments
    }

filterOutEofAndPragmas ::
     [GHC.GenLocated l GHC.EpaComment] -> [GHC.GenLocated l GHC.EpaComment]
filterOutEofAndPragmas = filter isNeitherEofNorPragmaComment

isNeitherEofNorPragmaComment :: GHC.GenLocated l GHC.EpaComment -> Bool
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
isNeitherEofNorPragmaComment (GHC.L _ (GHC.EpaComment GHC.EpaEofComment _)) =
  False
#endif
isNeitherEofNorPragmaComment (GHC.L _ (GHC.EpaComment token _)) =
  not $ isPragma token

mkCommentGroupFromTrailingComment :: GHC.LEpaComment -> CommentGroup
mkCommentGroupFromTrailingComment comment =
  mempty {commentsAfter = [mkComment comment]}
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCommentGroupFromCommentEntry :: CommentEntryLocation -> CommentGroup
mkCommentGroupFromCommentEntry = mkCommentGroupFromEpaLocation
#else
mkCommentGroupFromCommentEntry :: CommentEntryLocation -> CommentGroup
mkCommentGroupFromCommentEntry _ = mempty
#endif
mkCommentSourceFromData :: Data a => a -> CommentSource
mkCommentSourceFromData x =
  case (findCommentEntryLocation x, findEpAnnComments x) of
    (Just entry, Just comments) -> FromEntryAndComments entry comments
    (Nothing, Just comments) -> FromEpAnnComments comments
    (Just entry, Nothing) -> FromEntryLocation entry
    (Nothing, Nothing) -> NoComments

findCommentEntryLocation :: Data a => a -> Maybe CommentEntryLocation
findCommentEntryLocation x =
  cast x <|> gmapQl (<|>) Nothing findCommentEntryLocation x

findEpAnnComments :: Data a => a -> Maybe GHC.EpAnnComments
findEpAnnComments x = cast x <|> gmapQl (<|>) Nothing findEpAnnComments x
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCommentGroupFromEpAnnEntry :: GHC.EpaLocation -> CommentGroup
mkCommentGroupFromEpAnnEntry = mkCommentGroupFromEpaLocation
#else
mkCommentGroupFromEpAnnEntry :: Annotation.Anchor -> CommentGroup
mkCommentGroupFromEpAnnEntry _ = mempty
#endif
