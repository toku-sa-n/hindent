{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | AST with comments.
module HIndent.Ast.WithComments
  ( WithComments
  , mkWithCommentsWithEpAnn
  , mkWithCommentsWithGenLocated
  , printCommentsAnd
  , getNode
  ) where

import Control.Monad
import Control.Monad.RWS
import GHC.Hs
import GHC.Types.SrcLoc
import HIndent.Ast.Pragma
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
import HIndent.Printer

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

mkWithCommentsWithEpAnn :: EpAnn a -> b -> WithComments b
mkWithCommentsWithEpAnn ann =
  WithComments (epaComments $ filterOutEofAndPragmasFromAnn ann)

mkWithCommentsWithSrcAnn :: SrcAnn a -> b -> WithComments b
mkWithCommentsWithSrcAnn SrcSpanAnn {..} = mkWithCommentsWithEpAnn ann

mkWithCommentsWithGenLocated :: GenLocated (SrcAnn a) b -> WithComments b
mkWithCommentsWithGenLocated (L ann x) = mkWithCommentsWithSrcAnn ann x

getNode :: WithComments a -> a
getNode = node

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

printCommentsAnd :: WithComments a -> (a -> Printer ()) -> Printer ()
printCommentsAnd x f = do
  printCommentsBefore x
  f $ node x
  printCommentOnSameLine x
  printCommentsAfter x

-- | Prints comments that are before the given AST node.
printCommentsBefore :: CommentExtraction a => a -> Printer ()
printCommentsBefore p =
  forM_ (commentsBefore $ nodeComments p) $ \(L loc c) -> do
    let col = fromIntegral $ srcSpanStartCol (anchor loc) - 1
    indentedWithFixedLevel col $ pretty c
    newline

-- | Prints comments that are on the same line as the given AST node.
printCommentOnSameLine :: CommentExtraction a => a -> Printer ()
printCommentOnSameLine (commentsOnSameLine . nodeComments -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel
           (fromIntegral $ srcSpanStartCol $ anchor $ getLoc c)
           $ spaced
           $ fmap pretty
           $ c : cs
    else spacePrefixed $ fmap pretty $ c : cs
  eolCommentsArePrinted
printCommentOnSameLine _ = return ()

-- | Prints comments that are after the given AST node.
printCommentsAfter :: CommentExtraction a => a -> Printer ()
printCommentsAfter p =
  case commentsAfter $ nodeComments p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \(L loc c) -> do
        let col = fromIntegral $ srcSpanStartCol (anchor loc) - 1
        indentedWithFixedLevel col $ pretty c
        eolCommentsArePrinted
