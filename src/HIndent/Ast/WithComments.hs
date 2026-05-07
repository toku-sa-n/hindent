{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module HIndent.Ast.WithComments
  ( WithComments
  , prettyWith
  , fromGenLocated
  , fromEpAnn
  , mkWithComments
  , getNode
  , getComments
  , addComments
  , flattenComments
  ) where

import Control.Monad
import Control.Monad.RWS
import Data.Int (Int64)
#if MIN_VERSION_ghc_lib_parser(9, 8, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
import qualified GHC.Parser.Annotation as GHC
#endif
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Comment (Comment, getColumn, mkComment)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pragma
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Printer

data CommentGroup = CommentGroup
  { commentsBefore :: [Comment]
  , commentsOnSameLine :: [Comment]
  , commentsAfter :: [Comment]
  } deriving (Eq)

data WithComments a = WithComments
  { comments :: CommentGroup
  , node :: a
  } deriving (Foldable, Traversable, Eq)

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

instance Functor WithComments where
  fmap f WithComments {..} = WithComments comments (f node)

instance (Pretty a) => Pretty (WithComments a) where
  pretty withComments = prettyWith withComments pretty

-- | Prints comments included in the location information and then the
-- AST node body.
prettyWith :: WithComments a -> (a -> Printer ()) -> Printer ()
prettyWith WithComments {..} f = do
  printCommentsBefore comments
  f node
  printCommentOnSameLine comments
  printCommentsAfter comments

-- | Prints comments that are before the given AST node.
printCommentsBefore :: CommentGroup -> Printer ()
printCommentsBefore p =
  forM_ (commentsBefore p) $ \comment -> do
    indentedWithFixedLevel (getCommentColumn comment) $ pretty comment
    newline

-- | Prints comments that are on the same line as the given AST node.
printCommentOnSameLine :: CommentGroup -> Printer ()
printCommentOnSameLine (commentsOnSameLine -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel (getCommentColumn c)
           $ spaced
           $ fmap pretty
           $ c : cs
    else spacePrefixed $ fmap pretty $ c : cs
  eolCommentsArePrinted
printCommentOnSameLine _ = return ()

-- | Prints comments that are after the given AST node.
printCommentsAfter :: CommentGroup -> Printer ()
printCommentsAfter p =
  case commentsAfter p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \comment -> do
        indentedWithFixedLevel (getCommentColumn comment) $ pretty comment
        eolCommentsArePrinted
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
fromGenLocated :: GHC.GenLocated (GHC.EpAnn ann) a -> WithComments a
fromGenLocated (GHC.L ann a) = fromEpAnn ann a
#else
fromGenLocated ::
     GHC.GenLocated (GHC.SrcSpanAnn' (GHC.EpAnn ann)) a -> WithComments a
fromGenLocated (GHC.L ann a) = fromEpAnn (GHC.ann ann) a
#endif
fromEpAnn :: GHC.EpAnn a -> b -> WithComments b
fromEpAnn ann = WithComments (mkCommentGroupFromEpAnn ann)

mkWithComments :: a -> WithComments a
mkWithComments = WithComments mempty

getNode :: WithComments a -> a
getNode = node

getComments :: WithComments a -> CommentGroup
getComments = comments

flattenComments :: WithComments (WithComments a) -> WithComments a
flattenComments (WithComments outerComments (WithComments innerComments node)) =
  WithComments (outerComments <> innerComments) node

addComments :: CommentGroup -> WithComments a -> WithComments a
addComments extra (WithComments current node) =
  WithComments (extra <> current) node

getCommentColumn :: Comment -> Int64
getCommentColumn = fromIntegral . getColumn

mkCommentGroupFromEpAnn :: GHC.EpAnn a -> CommentGroup
mkCommentGroupFromEpAnn =
  mkCommentGroupFromEpAnn' . filterOutEofAndPragmasFromAnn

mkCommentGroupFromEpaLocation :: GHC.EpaLocation -> CommentGroup
mkCommentGroupFromEpaLocation GHC.EpaSpan {} = mempty
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCommentGroupFromEpaLocation (GHC.EpaDelta _ _ trailing) =
  foldMap mkCommentGroupFromTrailingComment trailing
#else
mkCommentGroupFromEpaLocation (GHC.EpaDelta _ trailing) =
  foldMap mkCommentGroupFromTrailingComment trailing
#endif
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
      GHC.srcSpanEndLine (epaLocationToRealSrcSpan entry)
        == GHC.srcSpanStartLine (epaLocationToRealSrcSpan commentLoc)
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
      GHC.srcSpanEndLine (epaLocationToRealSrcSpan entry)
        == GHC.srcSpanStartLine (epaLocationToRealSrcSpan commentLoc)
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
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCommentGroupFromEpAnnEntry :: GHC.EpaLocation -> CommentGroup
mkCommentGroupFromEpAnnEntry = mkCommentGroupFromEpaLocation
#else
mkCommentGroupFromEpAnnEntry :: GHC.Anchor -> CommentGroup
mkCommentGroupFromEpAnnEntry _ = mempty
#endif

#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
epaLocationToRealSrcSpan :: GHC.EpaLocation' a -> GHC.RealSrcSpan
epaLocationToRealSrcSpan = GHC.epaLocationRealSrcSpan
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
epaLocationToRealSrcSpan :: GHC.EpaLocation' a -> GHC.RealSrcSpan
epaLocationToRealSrcSpan = GHC.anchor
#else
epaLocationToRealSrcSpan :: GHC.Anchor -> GHC.RealSrcSpan
epaLocationToRealSrcSpan = GHC.anchor
#endif
