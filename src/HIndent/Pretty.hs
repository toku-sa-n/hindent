{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

data StmtOrComment
  = Stmt (LStmtLR GhcPs GhcPs (LHsExpr GhcPs))
  | Comment LEpaComment

-- | Pretty print including comments.
class Pretty a where
  pretty :: a -> Printer ()

instance Pretty HsModule where
  pretty m = do
    inter blankline printers
    printCommentsAtTheEndOfModule m
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists m, outputPragmas m)
        , (moduleDeclarationExists m, outputModuleDeclaration m)
        , (importsExist m, outputImports m)
        , (declsExist m, outputDecls)
        ]
      printCommentsAtTheEndOfModule =
        mapM_ (\x -> newline >> pretty x) .
        filter (not . isPragma) . listify (const True) . hsmodAnn
      outputDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp) $
        addSeparator $ unLoc <$> hsmodDecls m
      addSeparator []     = []
      addSeparator [x]    = [(x, Nothing)]
      addSeparator (x:xs) = (x, Just $ separator x) : addSeparator xs
      separator SigD {} = newline
      separator _       = blankline
      declsExist = not . null . hsmodDecls

instance Pretty e => Pretty (GenLocated l e) where
  pretty (L _ e) = pretty e

instance Pretty (HsDecl GhcPs) where
  pretty (TyClD _ d)    = pretty d
  pretty (InstD _ inst) = pretty inst
  pretty DerivD {}      = undefined
  pretty (ValD _ bind)  = pretty bind
  pretty (SigD _ s)     = insideSignature $ pretty s
  pretty KindSigD {}    = undefined
  pretty DefD {}        = undefined
  pretty ForD {}        = undefined
  pretty WarningD {}    = undefined
  pretty AnnD {}        = undefined
  pretty RuleD {}       = undefined
  pretty (SpliceD _ sp) = pretty sp
  pretty DocD {}        = undefined
  pretty RoleAnnotD {}  = undefined

instance Pretty (TyClDecl GhcPs) where
  pretty DataDecl {..} = do
    string "data "
    output tcdLName
    pretty tcdDataDefn
  pretty x = output x

instance Pretty (InstDecl GhcPs) where
  pretty ClsInstD {..} = pretty cid_inst
  pretty x             = output x

instance Pretty (HsBind GhcPs) where
  pretty FunBind {..} = pretty fun_matches
  pretty x            = output x

instance Pretty (Sig GhcPs) where
  pretty (TypeSig _ funName params) = do
    output $ unLoc $ head funName
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
  pretty x = output x

instance Pretty (HsDataDefn GhcPs) where
  pretty HsDataDefn {..} = do
    whenJust dd_kindSig $ \x -> do
      string " :: "
      output x
    string " where"
    indentedBlock $
      forM_ dd_cons $ \x -> do
        newline
        pretty x

instance Pretty (ClsInstDecl GhcPs) where
  pretty ClsInstDecl {..} = do
    string "instance "
    output cid_poly_ty
    unless (isEmptyBag cid_binds) $ do
      string " where"
      newline
      indentedBlock $ mapM_ pretty cid_binds

instance Pretty (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty MG {..} = inter (whenInsideCase newline) $ pretty <$> unLoc mg_alts

instance Pretty (HsExpr GhcPs) where
  pretty v@HsVar {} = prefixExpr v
  pretty HsUnboundVar {} = undefined
  pretty HsConLikeOut {} = undefined
  pretty HsRecFld {} = undefined
  pretty HsOverLabel {} = undefined
  pretty HsIPVar {} = undefined
  pretty full@HsOverLit {} = output full
  pretty (HsLit _ l) = output l
  pretty (HsLam _ body) = insideLambda $ pretty body
  pretty (HsLamCase _ matches) =
    insideCase $ do
      string "\\case"
      newline
      indentedBlock $ pretty matches
  pretty full@HsApp {} = output full
  pretty HsAppType {} = undefined
  pretty (OpApp _ l o r) = horizontal `ifFitsOnOneLineOrElse` vertical
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
  pretty NegApp {} = undefined
  pretty (HsPar _ expr) = parens $ pretty expr
  pretty SectionL {} = undefined
  pretty SectionR {} = undefined
  pretty ExplicitTuple {} = undefined
  pretty ExplicitSum {} = undefined
  pretty (HsCase _ cond arms) =
    insideCase $ do
      string "case "
      output cond
      string " of"
      newline
      indentedBlock $ pretty arms
  pretty HsIf {} = undefined
  pretty HsMultiIf {} = undefined
  pretty (HsLet _ binds exprs) = do
    string "let "
    output binds
    newline
    string " in "
    output exprs
  pretty (HsDo _ (DoExpr _) xs) = do
    string "do"
    newline
    indentedBlock $ inter newline $ output <$> unLoc xs
  -- While the name contains "Monad", this branch seems to be for list comprehensions.
  pretty (HsDo r MonadComp xs) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal =
        brackets $ do
          pretty $ last $ unLoc xs
          string " | "
          mapM_ pretty $ init $ unLoc xs
      vertical =
        insideVerticalList $
        case firstStmtAndOthers stmts of
          Just (lastStmt, others) -> do
            string "[ "
            pretty lastStmt
            newline
            forM_ (stmtsAndPrefixes others) $ \(p, x) -> do
              string p
              pretty x
              newline
            string "]"
          Nothing -> string "[]"
      stmtsAndPrefixes l = ("| ", head l) : fmap (\x -> (prefix x, x)) (tail l)
      prefix Stmt {}    = ", "
      prefix Comment {} = "  "
      stmts =
        sortByLocation $
        fmap Comment (listify (const True) r) ++ fmap Stmt (unLoc xs)
      firstStmtAndOthers = f []
        where
          f _ []                 = Nothing
          f zs (Stmt y:ys)       = Just (y, zs ++ ys)
          f zs (y@Comment {}:ys) = f (y : zs) ys
      sortByLocation = sortBy (compare `on` getLocation)
      getLocation (Stmt x)    = realSrcSpan $ locA $ getLoc x
      getLocation (Comment x) = anchor $ getLoc x
  pretty HsDo {} = undefined
  pretty ExplicitList {} = undefined
  pretty (RecordCon _ name fields) = horizontal `ifFitsOnOneLineOrElse` vertical
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
  pretty full@RecordUpd {} = output full
  pretty HsGetField {} = undefined
  pretty HsProjection {} = undefined
  pretty ExprWithTySig {} = undefined
  pretty ArithSeq {} = undefined
  pretty (HsBracket _ inner) = pretty inner
  pretty HsRnBracketOut {} = undefined
  pretty HsTcBracketOut {} = undefined
  pretty HsSpliceE {} = undefined
  pretty HsProc {} = undefined
  pretty HsStatic {} = undefined
  pretty HsTick {} = undefined
  pretty HsBinTick {} = undefined
  pretty HsPragE {} = undefined

instance Pretty (HsSigType GhcPs) where
  pretty HsSig {..} = pretty sig_body

instance Pretty (ConDecl GhcPs) where
  pretty ConDeclGADT {..} = horizontal `ifFitsOnOneLineOrElse` vertical
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
  pretty x = output x

instance Pretty (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty Match {..} = do
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
  pretty l@LastStmt {} = output l
  pretty full@(BindStmt _ pat body) =
    output full `ifFitsOnOneLineOrElse` do
      output pat
      string " <-"
      newline
      indentedBlock $ indentedWithSpace 2 $ output body -- 2 for "| "
  pretty ApplicativeStmt {} = undefined
  pretty BodyStmt {} = undefined
  pretty l@LetStmt {} = output l
  pretty (ParStmt _ xs _ _) = do
    inVertical <- gets psInsideVerticalList
    if inVertical
      then vertical
      else horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = inter (string " | ") $ fmap output xs
      vertical = inter (newline >> string "| ") $ fmap pretty xs
  pretty TransStmt {..} =
    inter (newline >> string ", ") $
    fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
  pretty RecStmt {} = undefined

instance Pretty StmtOrComment where
  pretty (Stmt x)    = pretty x
  pretty (Comment x) = pretty $ ac_tok $ unLoc x

instance Pretty (HsRecFields GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty HsRecFields {..} = horizontal `ifFitsOnOneLineOrElse` vertical
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
  pretty HsForAllTy {} = undefined
  pretty HsQualTy {} = undefined
  pretty x@HsTyVar {} = output x
  pretty (HsAppTy _ l r) = do
    pretty l
    space
    pretty r
  pretty HsAppKindTy {} = undefined
  pretty (HsFunTy _ _ a b) = do
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
        indentedWithSpace (-3) $ do
          string "-> "
          pretty b
  pretty HsListTy {} = undefined
  pretty full@HsTupleTy {} = output full
  pretty HsSumTy {} = undefined
  pretty (HsOpTy _ l op r) = do
    pretty l
    space
    whenInsideSignature $ string "'"
    infixOp $ unLoc op
    space
    pretty r
  pretty (HsParTy _ inside) = do
    string "("
    pretty inside
    string ")"
  pretty HsIParamTy {} = undefined
  pretty HsStarTy {} = undefined
  pretty HsKindSig {} = undefined
  pretty (HsSpliceTy _ sp) = do
    string "$"
    pretty sp
  pretty HsDocTy {} = undefined
  pretty HsBangTy {} = undefined
  pretty HsRecTy {} = undefined
  pretty (HsExplicitListTy _ _ xs) = do
    string "'[ "
    inter (string ", ") $ fmap pretty xs
    string "]"
  pretty HsExplicitTupleTy {} = undefined
  pretty HsTyLit {} = undefined
  pretty HsWildCardTy {} = undefined
  pretty XHsType {} = undefined

instance Pretty (HsConDeclGADTDetails GhcPs) where
  pretty (PrefixConGADT xs) =
    inter (string " -> ") $
    flip fmap xs $ \case
      (HsScaled _ x) -> output x
  pretty (RecConGADT xs) = do
    string "{ "
    inter (newline >> string ", ") $
      flip fmap (unLoc xs) $ \(L _ ConDeclField {..}) -> do
        output $ head cd_fld_names
        string " :: "
        output cd_fld_type
    string "}"

instance Pretty (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty GRHSs {..} = mapM_ pretty grhssGRHSs

instance Pretty (HsMatchContext GhcPs) where
  pretty FunRhs {..} = output mc_fun
  pretty CaseAlt     = return ()
  pretty LambdaExpr  = return ()
  pretty x           = output x

instance Pretty (ParStmtBlock GhcPs GhcPs) where
  pretty (ParStmtBlock _ xs _ _) = do
    inVertical <- gets psInsideVerticalList
    if inVertical
      then vertical
      else horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = inter (string ", ") $ fmap output xs
      vertical = inter (newline >> string ", ") $ fmap output xs

instance Pretty RdrName where
  pretty = prefixOp

instance Pretty (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty (GRHS _ _ (L _ (HsDo _ (DoExpr _) body))) = do
    unlessInsideLambda space
    rhsSeparator
    space
    string "do"
    newline
    indentedBlock $ inter newline $ output <$> unLoc body
  pretty (GRHS _ _ body) = horizontal `ifFitsOnOneLineOrElse` vertical
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

instance Pretty EpaCommentTok where
  pretty (EpaLineComment c)  = string c
  pretty (EpaBlockComment c) = string c
  pretty _                   = return ()

instance Pretty (SpliceDecl GhcPs) where
  pretty (SpliceDecl _ sp _) = pretty sp

instance Pretty (HsSplice GhcPs) where
  pretty HsTypedSplice {}             = undefined
  pretty (HsUntypedSplice _ _ _ body) = pretty body
  pretty HsQuasiQuote {}              = undefined
  pretty HsSpliced {}                 = undefined

instance Pretty (Pat GhcPs) where
  pretty p@WildPat {} = output p
  pretty p@VarPat {} = output p
  pretty p@LazyPat {} = output p
  pretty AsPat {} = undefined
  pretty (ParPat _ inner) = parens $ pretty inner
  pretty p@BangPat {} = output p
  pretty ListPat {} = undefined
  pretty (TuplePat _ pats _) = parens $ inter (string ", ") $ fmap pretty pats
  pretty SumPat {} = undefined
  pretty ConPat {..} =
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
  pretty ViewPat {} = undefined
  pretty p@SplicePat {} = output p
  pretty p@LitPat {} = output p
  pretty NPat {} = undefined
  pretty p@NPlusKPat {} = output p
  pretty SigPat {} = undefined

instance Pretty (HsBracket GhcPs) where
  pretty (ExpBr _ expr) = brackets $ wrapWithBars $ pretty expr
  pretty (PatBr _ expr) =
    brackets $ do
      string "p"
      wrapWithBars $ pretty expr
  pretty DecBrL {} = undefined
  pretty DecBrG {} = undefined
  pretty (TypBr _ expr) =
    brackets $ do
      string "t"
      wrapWithBars $ pretty expr
  pretty (VarBr _ True var) = do
    string "'"
    pretty var
  pretty (VarBr _ False var) = do
    string "''"
    pretty var
  pretty TExpBr {} = undefined

infixExpr :: HsExpr GhcPs -> Printer ()
infixExpr (HsVar _ bind) = infixOp $ unLoc bind
infixExpr x              = pretty x

prefixExpr :: HsExpr GhcPs -> Printer ()
prefixExpr (HsVar _ bind) = prefixOp $ unLoc bind
prefixExpr x              = pretty x
