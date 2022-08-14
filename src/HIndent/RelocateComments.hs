{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HIndent.RelocateComments
  ( relocateComments
  ) where

import           Control.Monad.State
import           Data.Function
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
  evalState
    (relocate $ removeComments $ sortExprLStmt $ resetSrcSpan m)
    allComments
    -- TODO: I don't fully understand how does `collectAllComments` of the
    -- original source code do, and this `relocate` is just a copy of it.
    -- Examine what it does, and change `relocate` properly.
  where
    relocate =
      relocateCommentsBefore >=>
      relocateCommentsSameLineRev >=>
      relocateCommentsSameLine >=> relocateCommentsAfter
    allComments = listify (not . isEofComment . ac_tok . unLoc) m

-- | This function resets the source span of the given module by searching
-- an 'EpaEofComment'.
--
-- This process is necessary because the module obtained by parsing
-- a source code with 'parseModule' only contains its start position.
resetSrcSpan :: HsModule -> HsModule
resetSrcSpan m@HsModule {hsmodAnn = ea@EpAnn {..}} =
  m {hsmodAnn = ea {entry = entry {anchor = newAnchor}}}
  where
    newAnchor =
      mkRealSrcSpan
        (realSrcSpanStart $ anchor entry)
        (realSrcSpanEnd $ anchor $ getLoc eofComment)
    eofComment =
      head $
      filter (isEofComment . ac_tok . unLoc) $ getFollowingComments comments
resetSrcSpan HsModule {hsmodAnn = EpAnnNotUsed} =
  error "The given `hsModule` does not have its source span information."

-- | This function sorts lists of statements in order their positions.
--
-- For example, the last element of 'HsDo' of 'HsExpr' is the element
-- before a bar, and the elements are not sorted by their locations. This
-- function fixes the orderings.
sortExprLStmt :: HsModule -> HsModule
sortExprLStmt m@HsModule {hsmodDecls = xs} = m {hsmodDecls = sorted}
  where
    sorted = everywhere (mkT sortByLoc) xs
    sortByLoc :: [ExprLStmt GhcPs] -> [ExprLStmt GhcPs]
    sortByLoc = sortBy (compare `on` srcSpanToRealSrcSpan . locA . getLoc)

-- | This function scans the given AST from top to bottom and locates
-- comments in the comment pool before each node on it.
relocateCommentsBefore :: HsModule -> WithComments HsModule
relocateCommentsBefore = everywhereM (applyM f)
  where
    f epa@EpAnn {..} =
      insertComments (isBefore $ anchor entry) insertPriorComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isBefore anc comAnc =
      srcSpanStartCol anc == 1 &&
      srcSpanStartCol comAnc == 1 &&
      srcSpanStartLine comAnc < srcSpanStartLine anc

-- | This function scans the given AST from top to bottom and locates
-- comments in the comment pool above each node on it. Comments are
-- stored in the 'followingComments' of 'EpaCommentsBalanced'.
relocateCommentsSameLine :: HsModule -> WithComments HsModule
relocateCommentsSameLine = everywhereM (applyM f)
  where
    f epa@EpAnn {..} =
      insertComments (isOnSameLine $ anchor entry) insertFollowingComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isOnSameLine anc comAnc = srcSpanStartLine comAnc == srcSpanEndLine anc

-- | This function scans the given AST from bottom to top and locates
-- comments in the comment pool above each node on it. Comments are
-- stored in the 'followingComments' of 'EpaCommentsBalanced'.
relocateCommentsSameLineRev :: HsModule -> WithComments HsModule
relocateCommentsSameLineRev = everywhereMr (applyM f)
  where
    f epa@EpAnn {..} =
      insertComments (isOnSameLine $ anchor entry) insertFollowingComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isOnSameLine anc comAnc =
      srcSpanStartLine comAnc == srcSpanStartLine anc &&
      srcSpanStartLine comAnc == srcSpanEndLine anc

-- | This function scans the given AST from bottom to top and locates
-- comments in the comment pool after each node on it.
relocateCommentsAfter :: HsModule -> WithComments HsModule
relocateCommentsAfter = everywhereMr (applyM f)
  where
    f epa@EpAnn {..} =
      insertComments (isAfter $ anchor entry) insertFollowingComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isAfter anc comAnc = srcSpanEndLine anc <= srcSpanStartLine comAnc

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

-- | Right-associative `everywhereM`
everywhereMr ::
     forall m. Monad m
  => GenericM m
  -> GenericM m
everywhereMr f = go
  where
    go :: GenericM m
    go = gmapMr go >=> f

-- | Right-associative `gmapM`.
gmapMr ::
     forall a m. (Data a, Monad m)
  => (forall d. Data d =>
                  d -> m d)
  -> a
  -> m a
gmapMr f = gfoldl k return
  where
    k :: Data d => m (d -> b) -> d -> m b
    k c x = do
      x' <- f x
      c' <- c
      return (c' x')

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

-- | This functions returns 'True' if the given token is an Eof comment,
-- and 'False' otherwise.
isEofComment :: EpaCommentTok -> Bool
isEofComment EpaEofComment = True
isEofComment _             = False
