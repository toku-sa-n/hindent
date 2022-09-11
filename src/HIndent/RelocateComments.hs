-- TODO: Split this module by creating 'RelocateComments.Preprocessing'.
-- TODO: This module is not so much for comment relocation as for
-- preprocessing. Rename the module and create a new one called
-- 'RelocateComments' inside it.
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HIndent.RelocateComments
  ( relocateComments
  ) where

import           Control.Applicative
import           Control.Monad.State
import           Data.Function
import           Data.List
import           Data.Maybe
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
relocateComments m = evalState (relocate $ preprocessing m) allComments
    -- TODO: I don't fully understand how does `collectAllComments` of the
    -- original source code do, and this `relocate` is just a copy of it.
    -- Examine what it does, and change `relocate` properly.
  where
    preprocessing =
      removeAllDocDs .
      closeEpAnnOfMatchMExt .
      closePlaceHolderEpAnns .
      closeEpAnnOfFunBindFunId .
      replaceAllNotUsedAnns . removeComments . sortExprLStmt . resetSrcSpan
    relocate =
      relocatePragmas >=>
      relocateCommentsBeforePragmas >=>
      relocateCommentsBefore >=>
      relocateCommentsSameLineRev >=>
      relocateCommentsSameLine >=>
      relocateCommentsTopLevelWhereClause >=> relocateCommentsAfter
    allComments = listify (not . isEofComment . ac_tok . unLoc) m

-- | This function resets the source span of the given module by searching
-- an 'EpaEofComment'.
--
-- This process is necessary because the module obtained by parsing
-- a source code with 'parseModule' only contains its start position.
--
-- TODO: Update the above comment. We need to specify column 1 to locate comments.
-- TODO: Split setting the start and last positiong.
-- TODO: Improve the name.
resetSrcSpan :: HsModule -> HsModule
resetSrcSpan m@HsModule { hsmodAnn = ea@EpAnn {..}
                        , hsmodName = Just (L (SrcSpanAnn _ (RealSrcSpan sp _)) _)
                        } =
  m {hsmodAnn = ea {entry = entry {anchor = newAnchor}}}
  where
    newAnchor =
      mkRealSrcSpan
        (mkRealSrcLoc (srcSpanFile sp) (srcSpanStartLine sp) 1)
        (realSrcSpanEnd $ anchor $ getLoc eofComment)
    eofComment =
      head $
      filter (isEofComment . ac_tok . unLoc) $ getFollowingComments comments
resetSrcSpan m = m

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

-- | This function locates pragmas to the module's EPA.
--
-- FIXME: Do we need 'everywhereM''?
relocatePragmas :: HsModule -> WithComments HsModule
relocatePragmas m@HsModule {hsmodAnn = hsmodAnn} = do
  newAnn <- everywhereM' (applyM f) hsmodAnn
  return m {hsmodAnn = newAnn}
  where
    f epa@EpAnn {} =
      insertComments (isPragma . ac_tok . unLoc) insertPriorComments epa
    f EpAnnNotUsed = pure EpAnnNotUsed

-- | This function locates comments that are located before pragmas.
relocateCommentsBeforePragmas :: HsModule -> WithComments HsModule
relocateCommentsBeforePragmas m@HsModule {hsmodAnn = hsmodAnn}
  | pragmaExists m = do
    newAnn <- everywhereM' (applyM f) hsmodAnn
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
relocateCommentsBefore = everywhereM' (applyM f)
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
relocateCommentsSameLine = everywhereM' (applyM f)
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
          getLocation (Sig x)    = realSrcSpan $ locA $ getLoc x
          getLocation (Method x) = realSrcSpan $ locA $ getLoc x
          newSigs =
            concatMap
              (\case
                 Sig x    -> [x]
                 Method _ -> [])
          newMethods =
            concatMap
              (\case
                 Sig _    -> []
                 Method x -> [x])
       in do newSigMethods <- mapM locateCommentsForSigMethod sigsMethods
             pure
               g
                 { grhssLocalBinds =
                     (HsValBinds
                        x
                        (ValBinds
                           x''
                           (listToBag $ newMethods newSigMethods)
                           (newSigs newSigMethods)))
                 }
    f x = pure x
    locateCommentsForSigMethod :: SigMethod -> WithComments SigMethod
    locateCommentsForSigMethod (Sig (L (SrcSpanAnn epa loc) sig)) = do
      cs <- get
      let (notAbove, above) = partitionAboveNotAbove cs (entry epa)
          newEpa = epa {comments = insertPriorComments (comments epa) above}
      put notAbove
      pure (Sig (L (SrcSpanAnn newEpa loc) sig))
    locateCommentsForSigMethod (Method (L (SrcSpanAnn epa loc) method)) = do
      cs <- get
      let (notAbove, above) = partitionAboveNotAbove cs (entry epa)
          newEpa = epa {comments = insertPriorComments (comments epa) above}
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

-- | This function removes all comments from the given module. It is necessary not to duplicate comments.
removeComments :: HsModule -> HsModule
removeComments = everywhere (mkT remover)
  where
    remover (EpaComments _)           = emptyComments
    remover (EpaCommentsBalanced _ _) = emptyComments

-- | This function replaces all 'EpAnnNotUsed's in 'SrcSpanAnn''s with
-- 'EpAnn's to make it possible to locate comments.
--
-- FIXME: This function has a lot of problems.
-- * The indentation of the implementation is too deep.
-- * The implementation is too long.
-- * The implementation does not generalize 'a' of 'EpAnn a'.
replaceAllNotUsedAnns :: HsModule -> HsModule
replaceAllNotUsedAnns = everywhere app
  where
    app ::
         forall a. Data a
      => (a -> a)
    app sp =
      case typeRep @a of
        App g (App y z) ->
          fromMaybe sp $ do
            HRefl <- eqTypeRep g (typeRep @SrcSpanAnn')
            HRefl <- eqTypeRep y (typeRep @EpAnn)
            let try :: Typeable b => b -> Maybe a
                try ann = do
                  HRefl <- eqTypeRep (typeOf ann) z
                  pure
                    sp {ann = EpAnn (spanAsAnchor (locA sp)) ann emptyComments}
            try emptyListItem <|> try emptyList <|> try emptyPragma <|>
              try emptyContext <|>
              try emptyNameAnn
        _ -> sp
    emptyListItem = AnnListItem []
    emptyList = AnnList Nothing Nothing Nothing [] []
    emptyPragma = AnnPragma emptyAddEpAnn emptyAddEpAnn []
    emptyContext = AnnContext Nothing [] []
    emptyNameAnn = NameAnnTrailing []
    emptyAddEpAnn = AddEpAnn AnnAnyclass emptyEpaLocation
    emptyEpaLocation = EpaDelta (SameLine 0) []

-- | This function replaces the 'EpAnn' of 'fun_id' in 'FunBind' with
-- 'EpAnnNotUsed'.
--
-- The 'fun_id' contains the function's name. However, 'FunRhs' of 'Match'
-- also contains the name, and we use the latter one. This function
-- prevents comments from being located in 'fun_id'.
closeEpAnnOfFunBindFunId :: HsModule -> HsModule
closeEpAnnOfFunBindFunId = everywhere (mkT closeEpAnn)
  where
    closeEpAnn :: HsBind GhcPs -> HsBind GhcPs
    closeEpAnn bind@FunBind {fun_id = (L (SrcSpanAnn _ l) name)} =
      bind {fun_id = L (SrcSpanAnn EpAnnNotUsed l) name}
    closeEpAnn x = x

-- | This function replaces the 'EpAnn' of 'm_ext' in 'Match' with
-- 'EpAnnNotUsed.
--
-- The field contains the annotation of the match LHS. However, the same
-- information is also stored inside the 'Match'. This function removes the
-- duplication not to locate comments on a wrong point.
closeEpAnnOfMatchMExt :: HsModule -> HsModule
closeEpAnnOfMatchMExt = everywhere closeEpAnn
  where
    closeEpAnn ::
         forall a. Typeable a
      => a
      -> a
    closeEpAnn x =
      case typeRep @a of
        App (App g h) _ ->
          case (eqTypeRep g (typeRep @Match), eqTypeRep h (typeRep @GhcPs)) of
            (Just HRefl, Just HRefl) -> x {m_ext = EpAnnNotUsed}
            _                        -> x
        _ -> x

-- | This function replaces all 'EpAnn's that contain placeholder anchors
-- to locate comments correctly. A placeholder anchor is an anchor pointing
-- (-1, -1).
closePlaceHolderEpAnns :: HsModule -> HsModule
closePlaceHolderEpAnns = everywhere (applyForEpAnn closeEpAnn)
  where
    closeEpAnn :: EpAnn a -> EpAnn a
    closeEpAnn (EpAnn (Anchor sp _) _ _)
      | srcSpanEndLine sp == -1 && srcSpanEndCol sp == -1 = EpAnnNotUsed
    closeEpAnn x = x

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

-- | This function removes all 'DocD's from the given module. They have
-- haddocks, but the same information is stored in 'EpaCommentTok's. Thus,
-- we need to remove the duplication.
removeAllDocDs :: HsModule -> HsModule
removeAllDocDs x@HsModule {hsmodDecls = decls} =
  x {hsmodDecls = filter (\(L _ r) -> not $ isDocD r) decls}
  where
    isDocD DocD {} = True
    isDocD _       = False

-- | 'everywhereM' in top-down manner.
everywhereM' :: GenericM WithComments -> GenericM WithComments
everywhereM' f = go
  where
    go :: GenericM WithComments
    go = f >=> gmapM go

data Wrapper =
  forall a. Typeable (EpAnn a) =>
            Wrapper (EpAnn a)

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

applyForEpAnn ::
     forall a. Typeable a
  => (forall b. EpAnn b -> EpAnn b)
  -> (a -> a)
applyForEpAnn f =
  case typeRep @a of
    App g _ ->
      case eqTypeRep g (typeRep @EpAnn) of
        Just HRefl -> f
        Nothing    -> id
    _ -> id

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

-- | This function sorts comments by its location.
sortCommentsByLocation :: [LEpaComment] -> [LEpaComment]
sortCommentsByLocation = sortBy (compare `on` anchor . getLoc)

-- | This functions returns 'True' if the given token is an Eof comment,
-- and 'False' otherwise.
isEofComment :: EpaCommentTok -> Bool
isEofComment EpaEofComment = True
isEofComment _             = False
