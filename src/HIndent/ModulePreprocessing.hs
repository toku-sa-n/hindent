{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Module preprocessing before pretty-printing.
module HIndent.ModulePreprocessing
  ( modifyASTForPrettyPrinting
  ) where

import           Control.Applicative
import           Data.Function
import           Data.List
import           Data.Maybe
import           Generics.SYB                                  hiding (GT,
                                                                typeOf, typeRep)
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.ModulePreprocessing.CommentRelocation
import           Type.Reflection

-- | This function modifies the given module AST for pretty-printing
-- easier.
modifyASTForPrettyPrinting :: HsModule -> HsModule
modifyASTForPrettyPrinting m = relocateComments (preprocessing m) allComments
  where
    preprocessing =
      removeAllDocDs .
      closeEpAnnOfMatchMExt .
      closePlaceHolderEpAnns .
      closeEpAnnOfFunBindFunId .
      replaceAllNotUsedAnns .
      removeComments . sortExprLStmt . resetModuleStartLine
    allComments = listify (not . isEofComment . ac_tok . unLoc) m

-- | This function sets the given module's start line as the module
-- name's start position if the name exists.
--
-- 'ghc-lib-parser''s parser sets the start position to the fixed point (1,
-- 1), and without correcting it, it is impossible to locate module
-- documentation above the module name.
resetModuleStartLine :: HsModule -> HsModule
resetModuleStartLine m@HsModule { hsmodAnn = epa@EpAnn {..}
                                , hsmodName = Just (L (SrcSpanAnn _ (RealSrcSpan sp _)) _)
                                } =
  m {hsmodAnn = epa {entry = entry {anchor = newAnchor}}}
  where
    newAnchor =
      mkRealSrcSpan
        (mkRealSrcLoc
           (srcSpanFile anc)
           (srcSpanStartLine sp)
           (srcSpanStartCol anc))
        (realSrcSpanEnd anc)
    anc = anchor entry
resetModuleStartLine m = m

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

-- | This function removes all comments from the given module not to
-- duplicate them on comment relocation.
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
    app sp
      | App g (App y z) <- typeRep @a =
        fromMaybe sp $ do
          HRefl <- eqTypeRep g (typeRep @SrcSpanAnn')
          HRefl <- eqTypeRep y (typeRep @EpAnn)
          let try :: Typeable b => b -> Maybe a
              try ann = do
                HRefl <- eqTypeRep (typeOf ann) z
                pure sp {ann = EpAnn (spanAsAnchor $ locA sp) ann emptyComments}
          try emptyListItem <|> try emptyList <|> try emptyPragma <|>
            try emptyContext <|>
            try emptyNameAnn
    app x = x
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
-- on (-1, -1).
closePlaceHolderEpAnns :: HsModule -> HsModule
closePlaceHolderEpAnns = everywhere (applyForEpAnn closeEpAnn)
  where
    closeEpAnn :: EpAnn a -> EpAnn a
    closeEpAnn (EpAnn (Anchor sp _) _ _)
      | srcSpanEndLine sp == -1 && srcSpanEndCol sp == -1 = EpAnnNotUsed
    closeEpAnn x = x

-- | This function removes all 'DocD's from the given module. They have
-- haddocks, but the same information is stored in 'EpaCommentTok's. Thus,
-- we need to remove the duplication.
removeAllDocDs :: HsModule -> HsModule
removeAllDocDs x@HsModule {hsmodDecls = decls} =
  x {hsmodDecls = filter (\(L _ r) -> not $ isDocD r) decls}
  where
    isDocD DocD {} = True
    isDocD _       = False

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
    -- 'HIndent.ModulePreprocessing.CommeentRelocation'.

-- | This functions returns 'True' if the given token is an Eof comment,
-- and 'False' otherwise.
isEofComment :: EpaCommentTok -> Bool
isEofComment EpaEofComment = True
isEofComment _             = False
