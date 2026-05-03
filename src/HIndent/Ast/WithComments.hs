{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Data (Data)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Typeable (cast)
import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Comment (mkComment)
import HIndent.Ast.NodeComments (NodeComments(..))
import qualified HIndent.Ast.NodeComments as NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Parser.Annotation as Annotation
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Printer

data WithComments a = WithComments
  { comments :: NodeComments
  , node :: a
  } deriving (Foldable, Traversable, Eq)

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
printCommentsBefore :: NodeComments -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
printCommentsBefore p =
  forM_ (commentsBefore p) $ \(GHC.L loc c) -> do
    let col =
          fromIntegral
            $ GHC.srcSpanStartCol (GHC.epaLocationRealSrcSpan loc) - 1
    indentedWithFixedLevel col $ pretty $ mkComment $ GHC.ac_tok c
    newline
#else
printCommentsBefore p =
  forM_ (commentsBefore p) $ \(GHC.L loc c) -> do
    let col = fromIntegral $ GHC.srcSpanStartCol (GHC.anchor loc) - 1
    indentedWithFixedLevel col $ pretty $ mkComment $ GHC.ac_tok c
    newline
#endif
-- | Prints comments that are on the same line as the given AST node.
printCommentOnSameLine :: NodeComments -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
printCommentOnSameLine (commentsOnSameLine -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel
           (fromIntegral
              $ GHC.srcSpanStartCol
              $ GHC.epaLocationRealSrcSpan
              $ GHC.getLoc c)
           $ spaced
           $ fmap prettyComment
           $ c : cs
    else spacePrefixed $ fmap prettyComment $ c : cs
  eolCommentsArePrinted
#else
printCommentOnSameLine (commentsOnSameLine -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel
           (fromIntegral $ GHC.srcSpanStartCol $ GHC.anchor $ GHC.getLoc c)
           $ spaced
           $ fmap prettyComment
           $ c : cs
    else spacePrefixed $ fmap prettyComment $ c : cs
  eolCommentsArePrinted
#endif
printCommentOnSameLine _ = return ()

-- | Prints comments that are after the given AST node.
printCommentsAfter :: NodeComments -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
printCommentsAfter p =
  case commentsAfter p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \(GHC.L loc c) -> do
        let col =
              fromIntegral
                $ GHC.srcSpanStartCol (GHC.epaLocationRealSrcSpan loc) - 1
        indentedWithFixedLevel col $ pretty $ mkComment $ GHC.ac_tok c
        eolCommentsArePrinted
#else
printCommentsAfter p =
  case commentsAfter p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \(GHC.L loc c) -> do
        let col = fromIntegral $ GHC.srcSpanStartCol (GHC.anchor loc) - 1
        indentedWithFixedLevel col $ pretty $ mkComment $ GHC.ac_tok c
        eolCommentsArePrinted
#endif
fromGenLocated :: Data l => GHC.GenLocated l a -> WithComments a
fromGenLocated (GHC.L l a) = WithComments (getLocatedComments l) a

fromEpAnn :: GHC.EpAnn a -> b -> WithComments b
fromEpAnn ann = WithComments (NodeComments.fromEpAnn ann)

mkWithComments :: a -> WithComments a
mkWithComments = WithComments mempty

getNode :: WithComments a -> a
getNode = node

getComments :: WithComments a -> NodeComments
getComments = comments

flattenComments :: WithComments (WithComments a) -> WithComments a
flattenComments (WithComments outerComments (WithComments innerComments node)) =
  WithComments (outerComments <> innerComments) node

addComments :: NodeComments -> WithComments a -> WithComments a
addComments extra (WithComments current node) =
  WithComments (extra <> current) node

prettyComment :: GHC.LEpaComment -> Printer ()
prettyComment (GHC.L _ comment) = pretty $ mkComment $ GHC.ac_tok comment

getLocatedComments :: Data l => l -> NodeComments
getLocatedComments location =
  fromMaybe
    (NodeComments.fromAnnotation location)
    (asum
       [ commentsFromSrcSpanAnnA location
       , commentsFromSrcSpanAnnL location
       , commentsFromSrcSpanAnnN location
       , commentsFromSrcSpanAnnP location
       , commentsFromSrcSpanAnnC location
       ])

commentsFromSrcSpanAnnA :: Data l => l -> Maybe NodeComments
commentsFromSrcSpanAnnA location =
  NodeComments.fromEpAnn . Annotation.srcSpanAnnAToEpAnn
    <$> (cast location :: Maybe Annotation.SrcSpanAnnA)

commentsFromSrcSpanAnnL :: Data l => l -> Maybe NodeComments
commentsFromSrcSpanAnnL location =
  NodeComments.fromEpAnn . Annotation.srcSpanAnnLToEpAnn
    <$> (cast location :: Maybe Annotation.SrcSpanAnnL)

commentsFromSrcSpanAnnN :: Data l => l -> Maybe NodeComments
commentsFromSrcSpanAnnN location =
  NodeComments.fromEpAnn . Annotation.srcSpanAnnNToEpAnn
    <$> (cast location :: Maybe Annotation.SrcSpanAnnN)

commentsFromSrcSpanAnnP :: Data l => l -> Maybe NodeComments
commentsFromSrcSpanAnnP location =
  NodeComments.fromEpAnn . Annotation.srcSpanAnnPToEpAnn
    <$> (cast location :: Maybe Annotation.SrcSpanAnnP)

commentsFromSrcSpanAnnC :: Data l => l -> Maybe NodeComments
commentsFromSrcSpanAnnC location =
  NodeComments.fromEpAnn . Annotation.srcSpanAnnCToEpAnn
    <$> (cast location :: Maybe Annotation.SrcSpanAnnC)
