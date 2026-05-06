{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
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

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.RWS
import Data.Data (Data, cast, gmapQl)
import Data.Int (Int64)
import qualified GHC.Hs as Hs
import qualified GHC.Types.SrcLoc as SrcLoc
import HIndent.Ast.Comment (Comment, getColumn, mkComment)
import qualified HIndent.GhcLibParserWrapper.GHC.Parser.Annotation as GHC
import HIndent.Pragma
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Printer

data CommentGroup = CommentGroup
  { commentsBefore :: [Comment]
  , commentsOnSameLine :: [Comment]
  , commentsAfter :: [Comment]
  } deriving (Eq)
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
type CommentEntryLocation = Hs.EpaLocation
#else
type CommentEntryLocation = GHC.Anchor
#endif
data CommentSource
  = forall a. FromEpAnn (Hs.EpAnn a)
  | FromEntryAndComments CommentEntryLocation Hs.EpAnnComments
  | FromEpAnnComments Hs.EpAnnComments
  | FromEntryLocation CommentEntryLocation
  | NoComments

class HasComments a where
  getCommentSource :: a -> CommentSource

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

instance HasComments Hs.EpAnnComments where
  getCommentSource = FromEpAnnComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance HasComments Hs.EpaLocation where
  getCommentSource = FromEntryLocation
#else
instance HasComments GHC.Anchor where
  getCommentSource = FromEntryLocation
#endif
instance HasComments Hs.EpAnnCO where
  getCommentSource = FromEpAnn

instance HasComments Hs.NoEpAnns where
  getCommentSource = const NoComments

instance HasComments SrcLoc.SrcSpan where
  getCommentSource = const NoComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance HasComments GHC.SrcSpanAnnA where
  getCommentSource = FromEpAnn

instance HasComments GHC.SrcSpanAnnL where
  getCommentSource = FromEpAnn

instance HasComments GHC.SrcSpanAnnN where
  getCommentSource = FromEpAnn

instance HasComments GHC.SrcSpanAnnP where
  getCommentSource = FromEpAnn

instance HasComments GHC.SrcSpanAnnC where
  getCommentSource = FromEpAnn
#else
instance HasComments GHC.SrcSpanAnnA where
  getCommentSource = FromEpAnn . GHC.ann

instance HasComments GHC.SrcSpanAnnL where
  getCommentSource = FromEpAnn . GHC.ann

instance HasComments GHC.SrcSpanAnnN where
  getCommentSource = FromEpAnn . GHC.ann

instance HasComments GHC.SrcSpanAnnP where
  getCommentSource = FromEpAnn . GHC.ann

instance HasComments GHC.SrcSpanAnnC where
  getCommentSource = FromEpAnn . GHC.ann
#endif
instance {-# OVERLAPPABLE #-} Data a => HasComments a where
  getCommentSource = mkCommentSourceFromData

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

fromGenLocated :: HasComments l => SrcLoc.GenLocated l a -> WithComments a
fromGenLocated (SrcLoc.L l a) = WithComments (mkCommentGroup l) a

fromEpAnn :: Hs.EpAnn a -> b -> WithComments b
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

mkCommentGroup :: HasComments a => a -> CommentGroup
mkCommentGroup = mkCommentGroupFromCommentSource . getCommentSource

mkCommentGroupFromEpAnn :: Hs.EpAnn a -> CommentGroup
mkCommentGroupFromEpAnn =
  mkCommentGroupFromEpAnn' . filterOutEofAndPragmasFromAnn

mkCommentGroupFromCommentSource :: CommentSource -> CommentGroup
mkCommentGroupFromCommentSource (FromEpAnn ann) = mkCommentGroupFromEpAnn ann
mkCommentGroupFromCommentSource (FromEntryAndComments entry comments) =
  mkCommentGroupFromEntryAndComments entry comments
mkCommentGroupFromCommentSource (FromEpAnnComments comments) =
  mkCommentGroupFromEpAnnComments comments
mkCommentGroupFromCommentSource (FromEntryLocation location) =
  mkCommentGroupFromCommentEntry location
mkCommentGroupFromCommentSource NoComments = mempty

mkCommentGroupFromEpAnnComments :: Hs.EpAnnComments -> CommentGroup
mkCommentGroupFromEpAnnComments comments =
  CommentGroup
    { commentsBefore = mkComment <$> Hs.priorComments filteredComments
    , commentsOnSameLine = []
    , commentsAfter = mkComment <$> Hs.getFollowingComments filteredComments
    }
  where
    filteredComments = filterOutEofAndPragmasFromComments comments

mkCommentGroupFromEntryAndComments ::
     CommentEntryLocation -> Hs.EpAnnComments -> CommentGroup
mkCommentGroupFromEntryAndComments entry comments =
  mkCommentGroupFromCommentEntry entry <> CommentGroup {..}
  where
    filteredComments = filterOutEofAndPragmasFromComments comments
    commentsBefore = mkComment <$> Hs.priorComments filteredComments
    commentsOnSameLine =
      fmap mkComment
        $ filter isCommentOnSameLine
        $ Hs.getFollowingComments filteredComments
    commentsAfter =
      fmap mkComment
        $ filter (not . isCommentOnSameLine)
        $ Hs.getFollowingComments filteredComments
    isCommentOnSameLine (SrcLoc.L commentLoc _) =
      SrcLoc.srcSpanEndLine (GHC.epaLocationToRealSrcSpan entry)
        == SrcLoc.srcSpanStartLine (GHC.epaLocationToRealSrcSpan commentLoc)

mkCommentGroupFromEpaLocation :: Hs.EpaLocation -> CommentGroup
mkCommentGroupFromEpaLocation Hs.EpaSpan {} = mempty
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCommentGroupFromEpaLocation (Hs.EpaDelta _ _ trailing) =
  foldMap mkCommentGroupFromTrailingComment trailing
#else
mkCommentGroupFromEpaLocation (Hs.EpaDelta _ trailing) =
  foldMap mkCommentGroupFromTrailingComment trailing
#endif
mkCommentGroupFromEpAnn' :: Hs.EpAnn a -> CommentGroup
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCommentGroupFromEpAnn' Hs.EpAnn {..} =
  mkCommentGroupFromEpAnnEntry entry <> CommentGroup {..}
  where
    commentsBefore = mkComment <$> Hs.priorComments comments
    commentsOnSameLine =
      fmap mkComment
        $ filter isCommentOnSameLine
        $ Hs.getFollowingComments comments
    commentsAfter =
      fmap mkComment
        $ filter (not . isCommentOnSameLine)
        $ Hs.getFollowingComments comments
    isCommentOnSameLine (SrcLoc.L commentLoc _) =
      SrcLoc.srcSpanEndLine (GHC.epaLocationToRealSrcSpan entry)
        == SrcLoc.srcSpanStartLine (GHC.epaLocationToRealSrcSpan commentLoc)
#else
mkCommentGroupFromEpAnn' Hs.EpAnn {..} =
  mkCommentGroupFromEpAnnEntry entry <> CommentGroup {..}
  where
    commentsBefore = mkComment <$> Hs.priorComments comments
    commentsOnSameLine =
      fmap mkComment
        $ filter isCommentOnSameLine
        $ Hs.getFollowingComments comments
    commentsAfter =
      fmap mkComment
        $ filter (not . isCommentOnSameLine)
        $ Hs.getFollowingComments comments
    isCommentOnSameLine (SrcLoc.L commentLoc _) =
      SrcLoc.srcSpanEndLine (GHC.epaLocationToRealSrcSpan entry)
        == SrcLoc.srcSpanStartLine (GHC.epaLocationToRealSrcSpan commentLoc)
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCommentGroupFromEpAnn' Hs.EpAnnNotUsed = mempty
#endif
#endif
filterOutEofAndPragmasFromAnn :: Hs.EpAnn ann -> Hs.EpAnn ann
filterOutEofAndPragmasFromAnn Hs.EpAnn {..} =
  Hs.EpAnn {comments = filterOutEofAndPragmasFromComments comments, ..}
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
filterOutEofAndPragmasFromAnn Hs.EpAnnNotUsed = Hs.EpAnnNotUsed
#endif
filterOutEofAndPragmasFromComments :: Hs.EpAnnComments -> Hs.EpAnnComments
filterOutEofAndPragmasFromComments comments =
  Hs.EpaCommentsBalanced
    { priorComments = filterOutEofAndPragmas $ Hs.priorComments comments
    , followingComments =
        filterOutEofAndPragmas $ Hs.getFollowingComments comments
    }

filterOutEofAndPragmas ::
     [SrcLoc.GenLocated l Hs.EpaComment] -> [SrcLoc.GenLocated l Hs.EpaComment]
filterOutEofAndPragmas = filter isNeitherEofNorPragmaComment

isNeitherEofNorPragmaComment :: SrcLoc.GenLocated l Hs.EpaComment -> Bool
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
isNeitherEofNorPragmaComment (SrcLoc.L _ (Hs.EpaComment Hs.EpaEofComment _)) =
  False
#endif
isNeitherEofNorPragmaComment (SrcLoc.L _ (Hs.EpaComment token _)) =
  not $ isPragma token

mkCommentGroupFromTrailingComment :: Hs.LEpaComment -> CommentGroup
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

findEpAnnComments :: Data a => a -> Maybe Hs.EpAnnComments
findEpAnnComments x = cast x <|> gmapQl (<|>) Nothing findEpAnnComments x
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCommentGroupFromEpAnnEntry :: Hs.EpaLocation -> CommentGroup
mkCommentGroupFromEpAnnEntry = mkCommentGroupFromEpaLocation
#else
mkCommentGroupFromEpAnnEntry :: GHC.Anchor -> CommentGroup
mkCommentGroupFromEpAnnEntry _ = mempty
#endif
