{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Comment relocation for pretty-printing them correctly.
--
-- HIndent gathers all comments above a function, an import, a module
-- declaration, etc. For example, HIndent formats the following code
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
-- AST nodes must have the information of which comments are above, on the
-- same line, and below. However, AST nodes generated by a parser of
-- 'ghc-lib-parser' only contain comments after them. 'relocateComments' is
-- defined to solve the problem.
module HIndent.ModulePreprocessing.CommentRelocation
  ( relocateComments
  ) where

import           Control.Monad.State
import           Data.Function
import           Data.List
import           Generics.SYB          hiding (GT, typeOf, typeRep)
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.Pretty.Pragma
import           Type.Reflection

-- | A wrapper type used in everywhereMEpAnnsBackwards' to collect all
-- 'EpAnn's to apply a function with them in order their positions.
data Wrapper =
  forall a. Typeable (EpAnn a) =>
            Wrapper (EpAnn a)

-- | 'State' with comments.
type WithComments = State [LEpaComment]

-- | This function collects all comments from the passed 'HsModule', and
-- modifies all 'EpAnn's so that all 'EpAnn's have 'EpaCommentsBalanced's.
--
-- This function solves the problem by collecting all 'LEpaComment's with
-- 'listify', and iterates all nodes from top to bottom a few times. During
-- the first iteration, this function adds comments above each node from
-- the collected ones to the node. On the next iteration, it adds a comment
-- on the same line. On the last iteration, it adds comments below them.
relocateComments :: HsModule -> [LEpaComment] -> HsModule
relocateComments = evalState . relocate
    -- TODO: I don't fully understand how does `collectAllComments` of the
    -- original source code do, and this `relocate` is just a copy of it.
    -- Examine what it does, and change `relocate` properly.
  where
    relocate =
      relocatePragmas >=>
      relocateCommentsBeforePragmas >=>
      relocateCommentsBeforeTopLevelDecls >=>
      relocateCommentsSameLineRev >=>
      relocateCommentsSameLineAfterNode >=>
      relocateCommentsTopLevelWhereClause >=> relocateCommentsAfter

-- | This function locates pragmas to the module's EPA.
relocatePragmas :: HsModule -> WithComments HsModule
relocatePragmas m@HsModule {hsmodAnn = epa@EpAnn {}} = do
  newAnn <- insertComments (isPragma . ac_tok . unLoc) insertPriorComments epa
  return m {hsmodAnn = newAnn}
relocatePragmas m = pure m

-- | This function locates comments that are located before pragmas to the
-- module's EPA.
relocateCommentsBeforePragmas :: HsModule -> WithComments HsModule
relocateCommentsBeforePragmas m@HsModule {hsmodAnn = ann}
  | pragmaExists m = do
    newAnn <- insertCommentsByPos (< startPosOfPragmas) insertPriorComments ann
    pure m {hsmodAnn = newAnn}
  | otherwise = pure m
  where
    startPosOfPragmas = anchor $ getLoc $ head $ priorComments $ comments ann

-- | This function locates comments located before top-level declarations.
relocateCommentsBeforeTopLevelDecls :: HsModule -> WithComments HsModule
relocateCommentsBeforeTopLevelDecls = everywhereM (applyM f)
  where
    f epa@EpAnn {..} =
      insertCommentsByPos (isBefore $ anchor entry) insertPriorComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isBefore anc comAnc =
      srcSpanStartCol anc == 1 &&
      srcSpanStartCol comAnc == 1 &&
      srcSpanStartLine comAnc < srcSpanStartLine anc

-- | This function locates comments that start from the same line of nodes'
-- end lines.
relocateCommentsSameLineAfterNode :: HsModule -> WithComments HsModule
relocateCommentsSameLineAfterNode = everywhereM (applyM f)
  where
    f epa@EpAnn {..} =
      insertCommentsByPos
        (isOnSameLine $ anchor entry)
        insertFollowingComments
        epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isOnSameLine anc comAnc = srcSpanStartLine comAnc == srcSpanEndLine anc

-- | This function scans the given AST from bottom to top and locates
-- comments in the comment pool above each node on it. Comments are
-- stored in the 'followingComments' of 'EpaCommentsBalanced'.
relocateCommentsSameLineRev :: HsModule -> WithComments HsModule
relocateCommentsSameLineRev = everywhereMEpAnnsBackwards f
  where
    f epa@EpAnn {..} =
      insertCommentsByPos
        (isOnSameLine $ anchor entry)
        insertFollowingComments
        epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isOnSameLine anc comAnc =
      srcSpanStartLine comAnc == srcSpanStartLine anc &&
      srcSpanStartLine comAnc == srcSpanEndLine anc

-- | This function locates comments above the top-level declarations in
-- a 'where' clause.
relocateCommentsTopLevelWhereClause :: HsModule -> WithComments HsModule
relocateCommentsTopLevelWhereClause = everywhereM (mkM f)
  where
    f :: GRHSs GhcPs (LHsExpr GhcPs)
      -> WithComments (GRHSs GhcPs (LHsExpr GhcPs))
    f g@GRHSs {grhssLocalBinds = (HsValBinds (EpAnn whereAnn AnnList {al_anchor = Just colAnc} _) ValBinds {})} =
      everywhereM (applyM modifyAnns) g
      where
        modifyAnns :: EpAnn a -> WithComments (EpAnn a)
        modifyAnns epa@EpAnn {..}
          | srcSpanStartCol (anchor entry) == srcSpanStartCol (anchor colAnc) =
            insertCommentsByPos (isAbove $ anchor entry) insertPriorComments epa
        modifyAnns x = pure x
        isAbove anc comAnc =
          srcSpanEndLine comAnc < srcSpanStartLine anc &&
          srcSpanStartLine (anchor whereAnn) <= srcSpanStartLine comAnc
    f x = pure x

-- | This function scans the given AST from bottom to top and locates
-- comments in the comment pool after each node on it.
relocateCommentsAfter :: HsModule -> WithComments HsModule
relocateCommentsAfter = everywhereMEpAnnsBackwards f
  where
    f epa@EpAnn {..} =
      insertCommentsByPos (isAfter $ anchor entry) insertFollowingComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    isAfter anc comAnc = srcSpanEndLine anc <= srcSpanStartLine comAnc

-- | This function applies the given function to all 'EpAnn's.
applyM ::
     forall a. Typeable a
  => (forall b. EpAnn b -> WithComments (EpAnn b))
  -> (a -> WithComments a)
applyM f
  | App g _ <- typeRep @a
  , Just HRefl <- eqTypeRep g (typeRep @EpAnn) = f
  | otherwise = pure

-- | This function drains comments whose positions satisfy the given
-- predicate and inserts them to the given node using the given inserter.
insertCommentsByPos ::
     (RealSrcSpan -> Bool)
  -> (EpAnnComments -> [LEpaComment] -> EpAnnComments)
  -> EpAnn a
  -> WithComments (EpAnn a)
insertCommentsByPos cond = insertComments (cond . anchor . getLoc)

-- | This function drains comments that satisfy the given predicate and
-- inserts them to the given node using the given inserter.
insertComments ::
     (LEpaComment -> Bool)
  -> (EpAnnComments -> [LEpaComment] -> EpAnnComments)
  -> EpAnn a
  -> WithComments (EpAnn a)
insertComments cond inserter epa@EpAnn {..} = do
  coms <- drainComments cond
  pure $ epa {comments = inserter comments coms}
insertComments _ _ EpAnnNotUsed = pure EpAnnNotUsed

-- | This function inserts comments to `priorComments`.
insertPriorComments :: EpAnnComments -> [LEpaComment] -> EpAnnComments
insertPriorComments (EpaComments prior) cs =
  EpaComments (sortCommentsByLocation $ prior ++ cs)
insertPriorComments (EpaCommentsBalanced prior following) cs =
  EpaCommentsBalanced (sortCommentsByLocation $ prior ++ cs) following

-- | This function inserts comments to `followingComments`.
insertFollowingComments :: EpAnnComments -> [LEpaComment] -> EpAnnComments
insertFollowingComments (EpaComments prior) cs = EpaCommentsBalanced prior cs
insertFollowingComments (EpaCommentsBalanced prior following) cs =
  EpaCommentsBalanced prior (sortCommentsByLocation $ following ++ cs)

-- | This function drains comments that satisfy the given predicate.
drainComments :: (LEpaComment -> Bool) -> WithComments [LEpaComment]
drainComments cond = do
  coms <- get
  let (xs, others) = partition cond coms
  put others
  return xs

-- | 'everywhereM' but applies the given function to EPAs in order their
-- positions from backwards.
--
-- FIXME: This code is too hard to read.
everywhereMEpAnnsBackwards ::
     (forall a. EpAnn a -> WithComments (EpAnn a))
  -> HsModule
  -> WithComments HsModule
everywhereMEpAnnsBackwards f hm = do
  epAnns <- collectEpAnnsInOrderEverywhereMTraverses
  results <- applyFunctionInOrderEPAEndPositions epAnns
  putModifiedEPAsToModule results
  where
    collectEpAnnsInOrderEverywhereMTraverses =
      reverse <$> execStateT (everywhereM collectEpAnnsST hm) []
    applyFunctionInOrderEPAEndPositions anns =
      forM sorted $ \(i, Wrapper x) -> do
        x' <- f x
        pure (i, Wrapper x')
      where
        indexed = zip [0 :: Int ..] anns
        sorted =
          sortBy
            (\(_, Wrapper a) (_, Wrapper b) -> compareEpaByEndPosition a b)
            indexed
    putModifiedEPAsToModule anns = evalStateT (everywhereM setEpAnn hm) [0 ..]
      where
        setEpAnn ::
             forall a. Typeable a
          => a
          -> StateT [Int] WithComments a
        setEpAnn x
          | App g g' <- typeRep @a
          , Just HRefl <- eqTypeRep g (typeRep @EpAnn) = do
            i <- gets head
            modify tail
            case lookup i anns of
              Just (Wrapper y)
                | App _ h <- typeOf y
                , Just HRefl <- eqTypeRep g' h -> pure y
              _ -> error "Unmatches"
          | otherwise = pure x
    collectEpAnnsST ::
         forall a. Typeable a
      => a
      -> StateT [Wrapper] WithComments a
    collectEpAnnsST x = do
      modify $ collectEpAnns x
      pure x
    collectEpAnns ::
         forall a. Typeable a
      => a
      -> ([Wrapper] -> [Wrapper])
    collectEpAnns x
      | App g _ <- typeRep @a
      , Just HRefl <- eqTypeRep g (typeRep @EpAnn) = (Wrapper x :)
      | otherwise = id

-- | This function sorts comments by its location.
sortCommentsByLocation :: [LEpaComment] -> [LEpaComment]
sortCommentsByLocation = sortBy (compare `on` anchor . getLoc)

-- | This function compares given EPAs by their end positions.
compareEpaByEndPosition :: EpAnn a -> EpAnn b -> Ordering
compareEpaByEndPosition (EpAnn a _ _) (EpAnn b _ _) =
  on compare (realSrcSpanEnd . anchor) b a
compareEpaByEndPosition EpAnnNotUsed EpAnnNotUsed = EQ
compareEpaByEndPosition _ EpAnnNotUsed = GT
compareEpaByEndPosition EpAnnNotUsed _ = LT
