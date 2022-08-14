{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Pretty printing.
module HIndent.Pretty
  ( pretty
  ) where

import           Control.Monad
import           Control.Monad.RWS
import           Data.Function
import           Data.List
import           Data.Maybe
import           Generics.SYB
import           GHC.Data.Bag
import           GHC.Hs
import           GHC.Types.Name.Reader
import           GHC.Types.SrcLoc
import           HIndent.Applicative
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Combinators.Inside
import           HIndent.Pretty.Combinators.Op
import           HIndent.Pretty.Combinators.Wrap
import           HIndent.Pretty.Imports
import           HIndent.Pretty.ModuleDeclaration
import           HIndent.Pretty.Pragma
import           HIndent.Types

data SigOrMethods
  = Sig (LSig GhcPs)
  | Method (LHsBindLR GhcPs GhcPs)

-- | Pretty print including comments.
--
-- TODO: Define `pretty` as a top-level function. It should have only one
-- definition, and it must not be changed in an instance declaration.
--
-- FIXME: 'Pretty' has a problem. It has two responsibilities; one is to
-- print a given node pretty, and the other is to collect comments from the
-- node.
--
-- Note that there are three types of nodes:
-- * A node that can pretty-print and has comments (e.g., 'HsModule')
-- * A node that can pretty-print but has no comments (e.g., almost all
-- nodes)
-- * A node that cannot pretty-print but has comments (e.g., 'EpAnn')
class Pretty a where
  pretty :: a -> Printer ()
  pretty p = do
    printCommentsBefore p
    pretty' p
    printCommentsSameLine p
    printCommentsAfter p
  pretty' :: a -> Printer ()
  printCommentsBefore :: a -> Printer ()
  printCommentsBefore = mapM_ pretty . commentsBefore
  printCommentsSameLine :: a -> Printer ()
  printCommentsSameLine = mapM_ pretty . commentsSameLine
  printCommentsAfter :: a -> Printer ()
  printCommentsAfter p =
    case commentsAfter p of
      [] -> return ()
      xs -> do
        isThereCommentsOnSameLine <- gets psEolComment
        unless isThereCommentsOnSameLine newline
        mapM_ pretty xs
  -- These functions must return comments that only this node can fetch. In
  -- other words, these functions must not return comments that child nodes
  -- can fetch.
  commentsBefore :: a -> [LEpaComment]
  commentsBefore = const []
  commentsSameLine :: a -> Maybe LEpaComment
  commentsSameLine = const Nothing
  commentsAfter :: a -> [LEpaComment]
  commentsAfter = const []

instance Pretty HsModule where
  pretty' m = inter blankline printers
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists m, outputPragmas m)
        , (moduleDeclarationExists m, outputModuleDeclaration m)
        , (importsExist m, outputImports m)
        , (declsExist m, outputDecls)
        ]
      outputDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp) $
        addSeparator $ hsmodDecls m
      addSeparator []     = []
      addSeparator [x]    = [(x, Nothing)]
      addSeparator (x:xs) = (x, Just $ separator $ unLoc x) : addSeparator xs
      separator SigD {} = newline
      separator _       = blankline
      declsExist = not . null . hsmodDecls
  commentsBefore =
    filter (not . isPragma . ac_tok . unLoc) .
    listify (not . isEofComment) . priorComments . comments . hsmodAnn
    where
      isEofComment (L _ (EpaComment EpaEofComment _)) = True
      isEofComment _                                  = False
  commentsAfter =
    filter (not . isPragma . ac_tok . unLoc) .
    followingComments . comments . hsmodAnn

-- FIXME: Requiring 'l' to implement 'Pretty' is wrong because some types
-- (e.g., 'EpAnn') cannot pretty-print. The restriction exists only for
-- extracting comments. Remove the restriction.
instance (Pretty l, Pretty e) => Pretty (GenLocated l e) where
  pretty' (L l e) = do
    printCommentsBefore l
    pretty e
    printCommentsSameLine l
    printCommentsAfter l

instance Pretty (HsDecl GhcPs) where
  pretty' (TyClD _ d)    = pretty d
  pretty' (InstD _ inst) = pretty inst
  pretty' DerivD {}      = undefined
  pretty' (ValD _ bind)  = pretty bind
  pretty' (SigD _ s)     = insideSignature $ pretty s
  pretty' KindSigD {}    = undefined
  pretty' DefD {}        = undefined
  pretty' ForD {}        = undefined
  pretty' WarningD {}    = undefined
  pretty' AnnD {}        = undefined
  pretty' RuleD {}       = undefined
  pretty' (SpliceD _ sp) = pretty sp
  pretty' DocD {}        = undefined
  pretty' RoleAnnotD {}  = undefined

instance Pretty (TyClDecl GhcPs) where
  pretty' DataDecl {..} = do
    string "data "
    output tcdLName
    pretty tcdDataDefn
  pretty' ClassDecl {..} = do
    string "class "
    pretty tcdLName
    space
    output tcdTyVars
    string " where"
    newline
    indentedBlock $ inter newline $ fmap pretty sigsAndMethods
    where
      sigsAndMethods =
        sortByLocation $ fmap Sig tcdSigs ++ fmap Method (bagToList tcdMeths)
      sortByLocation = sortBy (compare `on` getLocation)
      getLocation (Sig x)    = realSrcSpan $ locA $ getLoc x
      getLocation (Method x) = realSrcSpan $ locA $ getLoc x
  pretty' x = output x

instance Pretty (InstDecl GhcPs) where
  pretty' ClsInstD {..} = pretty cid_inst
  pretty' x             = output x

instance Pretty (HsBind GhcPs) where
  pretty' FunBind {..} = pretty fun_matches
  pretty' x            = output x

instance Pretty (Sig GhcPs) where
  pretty' (TypeSig _ funName params) = do
    pretty $ unLoc $ head funName
    horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        string " :: "
        pretty $ hswc_body params
      vertical =
        insideVerticalFunctionSignature $ do
          string " ::"
          newline
          indentedBlock $ indentedWithSpace 3 $ pretty $ hswc_body params -- 3 for "-> "
  pretty' (ClassOpSig _ isDefault funName params) = do
    when isDefault $ string "default "
    output $ unLoc $ head funName
    string " :: "
    pretty $ sig_body $ unLoc params
  pretty' x = output x

instance Pretty (HsDataDefn GhcPs) where
  pretty' HsDataDefn {..} = do
    whenJust dd_kindSig $ \x -> do
      string " :: "
      output x
    string " where"
    indentedBlock $
      forM_ dd_cons $ \x -> do
        newline
        pretty x

instance Pretty (ClsInstDecl GhcPs) where
  pretty' ClsInstDecl {..} = do
    string "instance "
    output cid_poly_ty
    unless (isEmptyBag cid_binds) $ do
      string " where"
      newline
      indentedBlock $ mapM_ pretty cid_binds

instance Pretty (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' MG {..} = inter (whenInsideCase newline) $ pretty <$> unLoc mg_alts

instance Pretty (HsExpr GhcPs) where
  pretty' v@HsVar {} = prefixExpr v
  pretty' HsUnboundVar {} = undefined
  pretty' HsConLikeOut {} = undefined
  pretty' HsRecFld {} = undefined
  pretty' HsOverLabel {} = undefined
  pretty' HsIPVar {} = undefined
  pretty' full@HsOverLit {} = output full
  pretty' (HsLit _ l) = output l
  pretty' (HsLam _ body) = insideLambda $ pretty body
  pretty' (HsLamCase _ matches) =
    insideCase $ do
      string "\\case"
      newline
      indentedBlock $ pretty matches
  pretty' full@HsApp {} = output full
  pretty' HsAppType {} = undefined
  pretty' (OpApp _ l o r) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = spaced [pretty l, infixExpr $ unLoc o, pretty r]
      vertical = do
        spaced [pretty l, infixExpr $ unLoc o]
        case unLoc r of
          (HsDo _ (DoExpr _) _) -> space
          HsLam {}              -> space
          HsLamCase {}          -> space
          _                     -> newline
        pretty r
  pretty' NegApp {} = undefined
  pretty' (HsPar _ expr) = parens $ pretty expr
  pretty' SectionL {} = undefined
  pretty' SectionR {} = undefined
  pretty' ExplicitTuple {} = undefined
  pretty' ExplicitSum {} = undefined
  pretty' (HsCase _ cond arms) =
    insideCase $ do
      string "case "
      output cond
      string " of"
      newline
      indentedBlock $ pretty arms
  pretty' HsIf {} = undefined
  pretty' HsMultiIf {} = undefined
  pretty' (HsLet _ binds exprs) = do
    string "let "
    output binds
    newline
    string " in "
    output exprs
  pretty' (HsDo _ (DoExpr _) xs) = do
    string "do"
    newline
    indentedBlock $ inter newline $ output <$> unLoc xs
  -- While the name contains "Monad", this branch seems to be for list comprehensions.
  pretty' (HsDo _ MonadComp xs) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal =
        brackets $ do
          pretty $ head $ unLoc xs
          string " | "
          mapM_ pretty $ tail $ unLoc xs
      vertical =
        insideVerticalList $
        if null $ unLoc xs
          then string "[]"
          else let (lastStmt, others) = (head $ unLoc xs, tail $ unLoc xs)
                in do string "[ "
                      pretty lastStmt
                      newline
                      forM_ (stmtsAndPrefixes others) $ \(p, x) -> do
                        indentedDependingOnHead (string p) $ pretty x
                        newline
                      string "]"
      stmtsAndPrefixes l = ("| ", head l) : fmap (", ", ) (tail l)
  pretty' HsDo {} = undefined
  pretty' ExplicitList {} = undefined
  pretty' (RecordCon _ name fields) =
    horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        name'
        space
        indentedBlock $ pretty fields
      vertical = do
        name'
        (space >> pretty fields) `ifFitsOnOneLineOrElse`
          (newline >> indentedBlock (pretty fields))
      name' =
        if head (showOutputable name) == ':'
          then parens $ output name
          else output name
  pretty' full@RecordUpd {} = output full
  pretty' HsGetField {} = undefined
  pretty' HsProjection {} = undefined
  pretty' ExprWithTySig {} = undefined
  pretty' ArithSeq {} = undefined
  pretty' (HsBracket _ inner) = pretty inner
  pretty' HsRnBracketOut {} = undefined
  pretty' HsTcBracketOut {} = undefined
  pretty' HsSpliceE {} = undefined
  pretty' HsProc {} = undefined
  pretty' HsStatic {} = undefined
  pretty' HsTick {} = undefined
  pretty' HsBinTick {} = undefined
  pretty' HsPragE {} = undefined

instance Pretty (HsSigType GhcPs) where
  pretty' HsSig {..} = pretty sig_body

instance Pretty (ConDecl GhcPs) where
  pretty' ConDeclGADT {..} = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        output $ head con_names
        string " :: "
        pretty con_g_args
        string " -> "
        output con_res_ty
      vertical = do
        output $ head con_names
        newline
        indentedBlock $ do
          indentedDependingOnHead (string ":: ") $ pretty con_g_args
          newline
          string "-> "
          output con_res_ty
  pretty' x = output x

instance Pretty (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' Match {..} = do
    isInsideCase <- gets psInsideCase
    isInsideLambda <- gets psInsideLambda
    whenInsideLambda $ string "\\"
    case (isInsideCase, isInsideLambda) of
      (True, _) -> do
        mapM_ pretty m_pats
        pretty m_grhss
      (_, True) -> do
        unless (null m_pats) $
          case unLoc $ head m_pats of
            LazyPat {} -> space
            BangPat {} -> space
            _          -> return ()
        mapM_ pretty m_pats
        space
        pretty m_grhss
      _ -> do
        pretty m_ctxt
        unless (null m_pats) $
          forM_ m_pats $ \x -> do
            space
            pretty x
        pretty m_grhss

instance Pretty (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' l@LastStmt {} = output l
  pretty' full@(BindStmt _ pat body) =
    output full `ifFitsOnOneLineOrElse` do
      output pat
      string " <-"
      newline
      indentedBlock $ output body
  pretty' ApplicativeStmt {} = undefined
  pretty' b@BodyStmt {} = output b
  pretty' (LetStmt _ l) = pretty l
  pretty' (ParStmt _ xs _ _) = do
    inVertical <- gets psInsideVerticalList
    if inVertical
      then vertical
      else horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = inter (string " | ") $ fmap output xs
      vertical = prefixedLined "| " $ fmap pretty xs
  pretty' TransStmt {..} =
    prefixedLined ", " $
    fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
  pretty' RecStmt {} = undefined
  commentsBefore (LetStmt l _) = commentsBefore l
  commentsBefore _             = []
  commentsAfter (LetStmt l _) = commentsAfter l
  commentsAfter _             = []

instance Pretty (HsRecFields GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' HsRecFields {..} = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        string "{"
        inter (string ", ") $ fmap output rec_flds
        string "}"
      vertical = do
        string "{ "
        inter (newline >> string ", ") $ fmap output rec_flds
        newline
        string "}"

instance Pretty (HsType GhcPs) where
  pretty' HsForAllTy {} = undefined
  pretty' HsQualTy {..} = do
    isInSig <- gets psInsideSignature
    if isInSig
      then sigHor `ifFitsOnOneLineOrElse` sigVer
      else notInSig
    where
      sigHor = do
        constraints
        string " => "
        pretty hst_body
      sigVer = do
        constraints
        newline
        indentedWithSpace (-3) $ string "=> "
        pretty hst_body
      notInSig = do
        constraints
        string " =>"
        newline
        indentedBlock $ pretty hst_body
      constraints =
        constraintsParens $
        mapM_ (inter (string ", ") . fmap pretty . unLoc) hst_ctxt
      constraintsParens =
        case hst_ctxt of
          Nothing        -> id
          Just (L _ [])  -> parens
          Just (L _ [_]) -> id
          Just _         -> parens
  pretty' x@HsTyVar {} = output x
  pretty' (HsAppTy _ l r) = do
    pretty l
    space
    pretty r
  pretty' HsAppKindTy {} = undefined
  pretty' (HsFunTy _ _ a b) = do
    isVertical <- gets psInsideVerticalFunctionSignature
    if isVertical
      then vertical
      else horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        pretty a
        string " -> "
        pretty b
      vertical = do
        pretty a
        newline
        indentedWithSpace (-3) $ string "-> "
        pretty b
  pretty' HsListTy {} = undefined
  pretty' full@HsTupleTy {} = output full
  pretty' HsSumTy {} = undefined
  -- For `HsOpTy`, we do not need a single quote for the infix operator. An
  -- explicit promotion is necessary if there is a data constructor and
  -- a type with the same name. However, infix data constructors never
  -- share their names with types because types cannot contain symbols.
  -- Thus there is no ambiguity.
  pretty' (HsOpTy _ l op r) = do
    pretty l
    space
    infixOp $ unLoc op
    space
    pretty r
  pretty' (HsParTy _ inside) = parens $ pretty inside
  pretty' t@HsIParamTy {} = output t
  pretty' HsStarTy {} = undefined
  pretty' HsKindSig {} = undefined
  pretty' (HsSpliceTy _ sp) = pretty sp
  pretty' HsDocTy {} = undefined
  pretty' HsBangTy {} = undefined
  pretty' HsRecTy {} = undefined
  pretty' (HsExplicitListTy _ _ xs) =
    case xs of
      [] -> string "'[]"
      _ -> do
        string "'[ "
        inter (string ", ") $ fmap pretty xs
        string "]"
  pretty' (HsExplicitTupleTy _ xs) = do
    string "'( "
    inter (string ", ") $ fmap pretty xs
    string ")"
  pretty' HsTyLit {} = undefined
  pretty' HsWildCardTy {} = undefined
  pretty' XHsType {} = undefined

instance Pretty (HsConDeclGADTDetails GhcPs) where
  pretty' (PrefixConGADT xs) =
    inter (string " -> ") $
    flip fmap xs $ \case
      (HsScaled _ x) -> output x
  pretty' (RecConGADT xs) = do
    string "{ "
    inter (newline >> string ", ") $
      flip fmap (unLoc xs) $ \(L _ ConDeclField {..}) -> do
        output $ head cd_fld_names
        string " :: "
        output cd_fld_type
    string "}"

instance Pretty (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' GRHSs {..} = mapM_ pretty grhssGRHSs

instance Pretty (HsMatchContext GhcPs) where
  pretty' FunRhs {..} = output mc_fun
  pretty' CaseAlt     = return ()
  pretty' LambdaExpr  = return ()
  pretty' x           = output x

instance Pretty (ParStmtBlock GhcPs GhcPs) where
  pretty' (ParStmtBlock _ xs _ _) = do
    inVertical <- gets psInsideVerticalList
    if inVertical
      then vertical
      else horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = inter (string ", ") $ fmap pretty xs
      vertical = prefixedLined ", " $ fmap pretty xs

instance Pretty RdrName where
  pretty' = prefixOp

instance Pretty (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' (GRHS _ _ (L _ (HsDo _ (DoExpr _) body))) = do
    unlessInsideLambda space
    rhsSeparator
    space
    string "do"
    newline
    indentedBlock $ inter newline $ output <$> unLoc body
  pretty' (GRHS _ _ body) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        unlessInsideLambda space
        rhsSeparator
        space
        pretty body
      vertical = do
        unlessInsideLambda space
        rhsSeparator
        newline
        indentedBlock $ pretty body
  commentsBefore (GRHS x _ _) = commentsBefore x
  commentsSameLine (GRHS x _ _) = commentsSameLine x
  commentsAfter (GRHS x _ _) = commentsAfter x

instance Pretty EpaCommentTok where
  pretty' (EpaLineComment c)  = string c >> commentsArePrinted
  pretty' (EpaBlockComment c) = string c >> commentsArePrinted
  pretty' _                   = return ()

instance Pretty (SpliceDecl GhcPs) where
  pretty' (SpliceDecl _ sp _) = pretty sp

instance Pretty (HsSplice GhcPs) where
  pretty' HsTypedSplice {} = undefined
  pretty' (HsUntypedSplice _ decoration _ body) = do
    string prefix
    pretty body
    where
      prefix =
        case decoration of
          DollarSplice -> "$"
          BareSplice   -> ""
  pretty' p@HsQuasiQuote {} = output p
  pretty' HsSpliced {} = undefined

instance Pretty (Pat GhcPs) where
  pretty' p@WildPat {} = output p
  pretty' p@VarPat {} = output p
  pretty' p@LazyPat {} = output p
  pretty' AsPat {} = undefined
  pretty' (ParPat _ inner) = parens $ pretty inner
  pretty' p@BangPat {} = output p
  pretty' ListPat {} = undefined
  pretty' (TuplePat _ pats _) = parens $ inter (string ", ") $ fmap pretty pats
  pretty' SumPat {} = undefined
  pretty' ConPat {..} =
    case pat_args of
      InfixCon a b -> do
        pretty a
        unlessSpecialOp (unLoc pat_con) space
        infixOp $ unLoc pat_con
        unlessSpecialOp (unLoc pat_con) space
        pretty b
      PrefixCon _ as -> do
        prefixOp $ unLoc pat_con
        space
        spaced $ fmap output as
      _ -> undefined
  pretty' ViewPat {} = undefined
  pretty' p@SplicePat {} = output p
  pretty' p@LitPat {} = output p
  pretty' NPat {} = undefined
  pretty' p@NPlusKPat {} = output p
  pretty' SigPat {} = undefined

instance Pretty (HsBracket GhcPs) where
  pretty' (ExpBr _ expr) = brackets $ wrapWithBars $ pretty expr
  pretty' (PatBr _ expr) =
    brackets $ do
      string "p"
      wrapWithBars $ pretty expr
  pretty' DecBrL {} = undefined
  pretty' DecBrG {} = undefined
  pretty' (TypBr _ expr) =
    brackets $ do
      string "t"
      wrapWithBars $ pretty expr
  pretty' (VarBr _ True var) = do
    string "'"
    pretty var
  pretty' (VarBr _ False var) = do
    string "''"
    pretty var
  pretty' TExpBr {} = undefined

instance Pretty SigOrMethods where
  pretty' (Sig x)    = pretty x
  pretty' (Method x) = pretty x

instance Pretty EpaComment where
  pretty' EpaComment {..} = pretty ac_tok

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't pretty-print 'Anchor'.
instance Pretty Anchor where
  pretty' _ = return ()

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't pretty-print 'SrcAnn'.
instance Pretty (SrcAnn a) where
  pretty' _ = return ()
  commentsBefore (SrcSpanAnn ep _) = commentsBefore ep
  commentsAfter (SrcSpanAnn ep _) = commentsAfter ep

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't pretty-print 'SrcSpan'.
instance Pretty SrcSpan where
  pretty' _ = return ()

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't pretty-print 'EpAnn'.
instance Pretty (EpAnn a) where
  pretty' _ = return ()
  commentsBefore (EpAnn _ _ cs) = priorComments cs
  commentsBefore EpAnnNotUsed   = []
  commentsAfter (EpAnn _ _ cs) = getFollowingComments cs
  commentsAfter EpAnnNotUsed   = []

instance Pretty (HsLocalBindsLR GhcPs GhcPs) where
  pretty' (HsValBinds _ lr) = do
    string "let "
    pretty lr
  pretty' x = output x

instance Pretty (HsValBindsLR GhcPs GhcPs) where
  pretty' (ValBinds _ ls _) = mapM_ pretty ls
  pretty' x                 = output x

infixExpr :: HsExpr GhcPs -> Printer ()
infixExpr (HsVar _ bind) = infixOp $ unLoc bind
infixExpr x              = pretty x

prefixExpr :: HsExpr GhcPs -> Printer ()
prefixExpr (HsVar _ bind) = prefixOp $ unLoc bind
prefixExpr x              = pretty x
