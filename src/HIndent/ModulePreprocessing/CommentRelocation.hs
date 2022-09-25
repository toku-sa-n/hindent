{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HIndent.ModulePreprocessing.CommentRelocation
  ( relocateComments
  ) where

import           Control.Monad.State
import           Data.Function
import           Data.List
import           Generics.SYB          hiding (GT, typeOf, typeRep)
import           GHC.Data.Bag
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.Pretty.Pragma
import           Type.Reflection

-- TODO: Merge this type with the same one in HIndent.Pretty'.
data SigMethod
  = Sig (LSig GhcPs)
  | Method (LHsBindLR GhcPs GhcPs)

data Wrapper =
  forall a. Typeable (EpAnn a) =>
            Wrapper (EpAnn a)

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
relocateComments :: HsModule -> [LEpaComment] -> HsModule
relocateComments = evalState . relocate
    -- TODO: I don't fully understand how does `collectAllComments` of the
    -- original source code do, and this `relocate` is just a copy of it.
    -- Examine what it does, and change `relocate` properly.
  where
    relocate =
      relocatePragmas >=>
      relocateCommentsBeforePragmas >=>
      relocateCommentsBefore >=>
      relocateCommentsSameLineRev >=>
      relocateCommentsSameLine >=>
      relocateCommentsTopLevelWhereClause >=> relocateCommentsAfter

-- | This function locates pragmas to the module's EPA.
relocatePragmas :: HsModule -> WithComments HsModule
relocatePragmas m@HsModule {hsmodAnn = epa@EpAnn {}} = do
  newAnn <- insertComments (isPragma . ac_tok . unLoc) insertPriorComments epa
  return m {hsmodAnn = newAnn}
relocatePragmas m@HsModule {hsmodAnn = EpAnnNotUsed} = pure m

-- | This function locates comments that are located before pragmas.
--
-- TODO: Do we need 'everywhereM'?
relocateCommentsBeforePragmas :: HsModule -> WithComments HsModule
relocateCommentsBeforePragmas m@HsModule {hsmodAnn = hsmodAnn}
  | pragmaExists m = do
    newAnn <- everywhereM (applyM f) hsmodAnn
    pure m {hsmodAnn = newAnn}
  | otherwise = pure m
  where
    f :: EpAnn a -> WithComments (EpAnn a)
    f epa@EpAnn {} =
      insertCommentsByPos (< startPosOfPragmas) insertPriorComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed
    startPosOfPragmas =
      anchor $ getLoc $ head $ priorComments $ comments hsmodAnn

-- | This function scans the given AST from top to bottom and locates
-- comments in the comment pool before each node on it.
relocateCommentsBefore :: HsModule -> WithComments HsModule
relocateCommentsBefore = everywhereM (applyM f)
  where
    f epa@EpAnn {..} =
      insertCommentsByPos (isBefore $ anchor entry) insertPriorComments epa
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
relocateCommentsSameLineRev = everywhereMr f
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

relocateCommentsTopLevelWhereClause :: HsModule -> WithComments HsModule
relocateCommentsTopLevelWhereClause = everywhereM (mkM f)
  where
    f :: GRHSs GhcPs (LHsExpr GhcPs)
      -> WithComments (GRHSs GhcPs (LHsExpr GhcPs))
    f g@GRHSs {grhssLocalBinds = (HsValBinds x (ValBinds x'' methods sigs))} =
      let sigsMethods =
            sortByLocation $ fmap Sig sigs ++ fmap Method (bagToList methods)
          sortByLocation = sortBy (compare `on` getLocation)
          getLocation (Sig x')    = realSrcSpan $ locA $ getLoc x'
          getLocation (Method x') = realSrcSpan $ locA $ getLoc x'
          newSigs =
            concatMap
              (\case
                 Sig x'   -> [x']
                 Method _ -> [])
          newMethods =
            concatMap
              (\case
                 Sig _     -> []
                 Method x' -> [x'])
       in do newSigMethods <- mapM locateCommentsForSigMethod sigsMethods
             pure
               g
                 { grhssLocalBinds =
                     HsValBinds
                       x
                       (ValBinds
                          x''
                          (listToBag $ newMethods newSigMethods)
                          (newSigs newSigMethods))
                 }
    f x = pure x
    locateCommentsForSigMethod :: SigMethod -> WithComments SigMethod
    locateCommentsForSigMethod (Sig (L (SrcSpanAnn epa loc) sig)) = do
      cs <- get
      let (notAbove, above) = partitionAboveNotAbove cs (entry epa)
          newEpa =
            case epa of
              EpAnn {} ->
                epa {comments = insertPriorComments (comments epa) above}
              _ -> undefined
      put notAbove
      pure (Sig (L (SrcSpanAnn newEpa loc) sig))
    locateCommentsForSigMethod (Method (L (SrcSpanAnn epa loc) method)) = do
      cs <- get
      let (notAbove, above) = partitionAboveNotAbove cs (entry epa)
          newEpa =
            case epa of
              EpAnn {} ->
                epa {comments = insertPriorComments (comments epa) above}
              _ -> undefined
      put notAbove
      pure (Method (L (SrcSpanAnn newEpa loc) method))
    partitionAboveNotAbove cs sp =
      fst $
      foldr
        (\c@(L comSp _) ((ls, rs), lastSpan) ->
           if anchor comSp `isAbove` anchor lastSpan
             then ((ls, c : rs), comSp)
             else ((c : ls, rs), lastSpan))
        (([], []), sp) $
      sortBy (compare `on` getLoc) cs
    isAbove comAnc anc =
      srcSpanStartCol comAnc == srcSpanStartCol anc &&
      srcSpanEndLine comAnc + 1 == srcSpanStartLine anc

-- | This function scans the given AST from bottom to top and locates
-- comments in the comment pool after each node on it.
relocateCommentsAfter :: HsModule -> WithComments HsModule
relocateCommentsAfter = everywhereMr f
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
applyM f =
  case typeRep @a of
    App g _ ->
      case eqTypeRep g (typeRep @EpAnn) of
        Just HRefl -> f
        Nothing    -> pure
    _ -> pure

insertComments ::
     (LEpaComment -> Bool)
  -> (EpAnnComments -> [LEpaComment] -> EpAnnComments)
  -> EpAnn a
  -> WithComments (EpAnn a)
insertComments cond inserter epa@EpAnn {..} = do
  coms <- drainComments cond
  pure $ epa {comments = inserter comments coms}
insertComments _ _ EpAnnNotUsed = pure EpAnnNotUsed

-- | This function drains comments whose positions satisfy the given
-- predicate and inserts them to the given node using the given inserter.
-- TODO: Remove duplications with 'insertComments'
insertCommentsByPos ::
     (RealSrcSpan -> Bool)
  -> (EpAnnComments -> [LEpaComment] -> EpAnnComments)
  -> EpAnn a
  -> WithComments (EpAnn a)
insertCommentsByPos cond inserter epa@EpAnn {..} = do
  coms <- drainCommentsByPos cond
  pure $ epa {comments = inserter comments coms}
insertCommentsByPos _ _ EpAnnNotUsed = pure EpAnnNotUsed

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

-- | This function drains comments that satisfy the given predicate about
-- their positions.
drainCommentsByPos :: (RealSrcSpan -> Bool) -> WithComments [LEpaComment]
drainCommentsByPos cond =
  drainComments (\(L commentAnchor _) -> cond $ anchor commentAnchor)

-- | Right-associative 'everywhereM' in top-down manner.
--
-- FIXME: This code is too hard to read.
everywhereMr ::
     (forall a. EpAnn a -> WithComments (EpAnn a))
  -> HsModule
  -> WithComments HsModule
everywhereMr f hm = do
  let collectEpAnn ::
           forall a. Typeable a
        => a
        -> ([Wrapper] -> [Wrapper])
      collectEpAnn x =
        case typeRep @a of
          App g _ ->
            case eqTypeRep g (typeRep @EpAnn) of
              Just HRefl -> (Wrapper x :)
              Nothing    -> id
          _ -> id
      st ::
           forall a. Typeable a
        => a
        -> StateT [Wrapper] WithComments a
      st x = do
        modify $ collectEpAnn x
        pure x
  epAnns <- reverse <$> execStateT (everywhereM st hm) []
  let indexed = zip [0 :: Int ..] epAnns
      sorted = sortBy (\(_, Wrapper a) (_, Wrapper b) -> cmp a b) indexed
  results <-
    forM sorted $ \(i, Wrapper x) -> do
      x' <- f x
      pure (i, Wrapper x')
  let setEpAnn ::
           forall a. Typeable a
        => a
        -> StateT [Int] WithComments a
      setEpAnn x =
        case typeRep @a of
          App g g' ->
            case eqTypeRep g (typeRep @EpAnn) of
              Just HRefl -> do
                i <- gets head
                modify tail
                case lookup i results of
                  Just (Wrapper y) ->
                    case typeOf y of
                      App _ h ->
                        case eqTypeRep g' h of
                          Just HRefl -> pure y
                          Nothing    -> error "Unmatched."
                  Nothing -> error "Unmatched."
              Nothing -> pure x
          _ -> pure x
  evalStateT (everywhereM setEpAnn hm) [0 ..]
  where
    cmp :: EpAnn a -> EpAnn b -> Ordering
    cmp (EpAnn a _ _) (EpAnn b _ _) = on compare (realSrcSpanEnd . anchor) b a
    cmp EpAnnNotUsed EpAnnNotUsed   = EQ
    cmp _ EpAnnNotUsed              = GT
    cmp EpAnnNotUsed _              = LT

-- | This function sorts comments by its location.
sortCommentsByLocation :: [LEpaComment] -> [LEpaComment]
sortCommentsByLocation = sortBy (compare `on` anchor . getLoc)
