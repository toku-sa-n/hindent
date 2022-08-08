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
relocateComments m =
  evalState (relocate (removeComments $ resetSrcSpan m)) allComments
  where
    relocate =
      relocateCommentsBefore >=>
      relocateCommentsSameLine >=> relocateCommentsAfter
    allComments = listify (const True) m

-- | This function resets the source span of the given module by searching
-- an 'EpaEofComment'.
--
-- This process is necessary because the module obtained by parsing
-- a source code with 'parseModule' only contains its start position.
resetSrcSpan :: HsModule -> HsModule
resetSrcSpan m@HsModule {hsmodAnn = ea@EpAnn {..}} = m {hsmodAnn = newAnn}
  where
    newAnn = ea {entry = entry {anchor = newSp}}
    newSp =
      mkRealSrcSpan
        (realSrcSpanStart $ anchor entry)
        (realSrcSpanEnd $ anchor $ getLoc eofComment)
    eofComment =
      head $
      filter (isEofComment . ac_tok . unLoc) $ getFollowingComments comments
    isEofComment EpaEofComment = True
    isEofComment _             = False
resetSrcSpan HsModule {hsmodAnn = EpAnnNotUsed} =
  error "The given `hsModule` does not have its source span information."

-- | This function scans the given AST from top to bottom and locates
-- comments in the comment pool before each node on it.
relocateCommentsBefore :: HsModule -> WithComments HsModule
relocateCommentsBefore = everywhereM' (applyM f)
  where
    f epa@EpAnn {..} =
      insertComments (isBefore $ anchor entry) insertPriorComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isBefore anc comAnc = srcSpanEndLine comAnc < srcSpanStartLine anc

-- | This function scans the given AST from top to bottom and locates
-- comments in the comment pool above each node on it. Comments are
-- stored in the 'followingComments' of 'EpaCommentsBalanced'.
relocateCommentsSameLine :: HsModule -> WithComments HsModule
relocateCommentsSameLine = everywhereM' (applyM f)
  where
    f epa@EpAnn {..} =
      insertComments (isOnSameLine $ anchor entry) insertFollowingComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isOnSameLine anc comAnc = srcSpanStartLine anc == srcSpanStartLine comAnc

-- | This function scans the given AST from bottom to top and locates
-- comments in the comment pool after each node on it.
relocateCommentsAfter :: HsModule -> WithComments HsModule
relocateCommentsAfter = everywhereM (applyM f)
  where
    f epa@EpAnn {..} =
      insertComments (isAfter $ anchor entry) insertFollowingComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isAfter anc comAnc = srcSpanEndLine anc < srcSpanStartLine comAnc

-- | This function drains comments whose positions satisfy the given
-- predicate and inserts them to the given node using the given inserter.
insertComments ::
     (RealSrcSpan -> Bool)
  -> (EpAnnComments -> [LEpaComment] -> EpAnnComments)
  -> EpAnn a
  -> WithComments (EpAnn a)
insertComments cond inserter epa@EpAnn {..} = do
  coms <- drainComments cond
  pure $ epa {comments = inserter comments coms}
insertComments _ _ EpAnnNotUsed = pure EpAnnNotUsed

-- | This function inserts comments to `priorComments`.
insertPriorComments :: EpAnnComments -> [LEpaComment] -> EpAnnComments
insertPriorComments (EpaComments prior) cs = EpaComments (prior ++ cs)
insertPriorComments (EpaCommentsBalanced prior following) cs =
  EpaCommentsBalanced (prior ++ cs) following

-- | This function inserts comments to `followingComments`.
insertFollowingComments :: EpAnnComments -> [LEpaComment] -> EpAnnComments
insertFollowingComments (EpaComments prior) cs = EpaCommentsBalanced prior cs
insertFollowingComments (EpaCommentsBalanced prior following) cs =
  EpaCommentsBalanced prior (following ++ cs)

-- | This function removes all comments from the given module. It is necessary not to duplicate comments.
removeComments :: HsModule -> HsModule
removeComments = everywhere (mkT remover)
  where
    remover (EpaComments _)           = emptyComments
    remover (EpaCommentsBalanced _ _) = emptyComments

-- | This function drains comments that satisfy the given predicate.
drainComments :: (RealSrcSpan -> Bool) -> WithComments [LEpaComment]
drainComments cond = do
  coms <- get
  let (xs, others) =
        partition (\(L commentAnchor _) -> cond $ anchor commentAnchor) coms
  put others
  pure xs

-- | Monadic variation on 'everywhere''.
everywhereM' ::
     forall m. Monad m
  => GenericM m
  -> GenericM m
everywhereM' f = go
  where
    go :: GenericM m
    go = f >=> gmapM go

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