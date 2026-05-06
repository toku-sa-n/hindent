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
import Data.Maybe (fromMaybe)
import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as SrcLoc
import HIndent.Ast.Comment (Comment, getColumn)
import HIndent.Ast.CommentGroup
  ( CommentGroup(..)
  , HasComments
  , mkCommentGroup
  , mkCommentGroupFromEpAnn
  )
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Printer

data WithComments a = WithComments
  { comments :: CommentGroup
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
getCommentColumn = fromIntegral . fromMaybe 0 . getColumn
