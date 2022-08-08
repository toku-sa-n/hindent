{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HIndent.RelocateComments
  ( relocateComments
  ) where

import           Control.Monad.State
import           Data.List
import           Generics.SYB        hiding (typeRep)
import           GHC.Hs
import           GHC.Types.SrcLoc
import           Type.Reflection

-- | A state type with comments.
type WithComments = State [LEpaComment]

-- | This function collects all comments from the passed 'HsModule', and modifies all 'EpAnn's so that all 'EpAnn's have 'EpaCommentsBalanced's.
--
-- HIndent gathers all comments above a function, an import, a module declaration, etc. For example, HIndent formats the following code
--
-- > f :: Int
-- > f = 1
-- >
-- > -- A comment between f and g
-- >
-- > -- Another comment between f and g
-- >
-- > g :: Int
-- > g = 2
--
-- to
--
-- > f :: Int
-- > f = 1
-- >
-- > -- A comment between f and g
-- > -- Another comment between f and g
-- > g :: Int
-- > g = 2
--
-- AST's nodes must have the information of which comments are above, on the same line, and below. However, the 'HsModule's AST nodes obtained by parsing a module contain only comments after the nodes.
--
-- This function solves the problem by collecting all 'LEpaComment's with 'listify', and iterates all nodes from top to bottom a few times. During the first iteration, this function adds comments above each node from the collected ones to the node. On the next iteration, it adds a comment on the same line. On the last iteration, it adds comments below them.
relocateComments :: HsModule -> HsModule
relocateComments m = evalState (st m) allComments
  where
    st =
      relocateCommentsBefore >=>
      relocateCommentsSameLine >=> relocateCommentsAfter
    allComments = listify (const True) m

-- | This function scans the given AST from top to bottom and locates
-- comments in the comment pool before each node on it.
relocateCommentsBefore :: HsModule -> WithComments HsModule
relocateCommentsBefore = everywhereM (applyM f)
  where
    f :: EpAnn a -> State [LEpaComment] (EpAnn a)
    f epa@EpAnn {..} = do
      coms <- get
      let (others, xs) =
            partition (\(L commentAnchor _) -> entry < commentAnchor) coms
      put others
      pure $ epa {comments = setFollowingComments comments xs}
    f EpAnnNotUsed = pure EpAnnNotUsed

-- | This function scans the given AST from top to bottom and locates
-- comments in the comment pool above each node on it. Comments are
-- stored in the 'followingComments' of 'EpaCommentsBalanced'.
relocateCommentsSameLine :: HsModule -> WithComments HsModule
relocateCommentsSameLine = pure

-- | This function scans the given AST from bottom to top and locates
-- comments in the comment pool after each node on it.
relocateCommentsAfter :: HsModule -> WithComments HsModule
relocateCommentsAfter = pure

-- | This function applies the given function to all 'EpAnn's.
applyM ::
     forall a. Typeable a
  => (forall b. EpAnn b -> WithComments (EpAnn b))
  -> (a -> WithComments a)
applyM f =
  case typeRep @a of
    App g _ ->
      case eqTypeRep g (typeRep @EpAnn) of
        Just HRefl -> f
        Nothing    -> pure
    _ -> pure
