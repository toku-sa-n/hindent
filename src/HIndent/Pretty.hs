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
  pretty (ValD _ bind)  = pretty bind
  pretty (SigD _ s)     = insideSignature $ pretty s
  pretty x              = output x

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
    string " :: "
    pretty $ hswc_body params
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
  pretty MG {..} = do
    isInsideCase <- gets psInsideCase
    inter (when isInsideCase newline) $ pretty <$> unLoc mg_alts

instance Pretty (HsExpr GhcPs) where
  pretty (HsVar _ v) = output v
  pretty HsUnboundVar {} = undefined
  pretty HsConLikeOut {} = undefined
  pretty HsRecFld {} = undefined
  pretty HsOverLabel {} = undefined
  pretty HsIPVar {} = undefined
  pretty full@HsOverLit {} = output full
  pretty (HsLit _ l) = output l
  pretty full@HsLam {} = output full
  pretty HsLamCase {} = undefined
  pretty full@HsApp {} = output full
  pretty HsAppType {} = undefined
  pretty full@(OpApp _ l o r) =
    output full `ifFitsOnOneLineOrElse` do
      newline
      indentedBlock $ do
        pretty l
        string " "
        pretty o
        newline
        pretty r
  pretty NegApp {} = undefined
  pretty HsPar {} = undefined
  pretty SectionL {} = undefined
  pretty SectionR {} = undefined
  pretty ExplicitTuple {} = undefined
  pretty ExplicitSum {} = undefined
  pretty (HsCase _ cond arms) =
    insideCase $ do
      newline
      indentedBlock $ do
        string "case "
        output cond
        string " of"
        newline
        indentedBlock $ pretty arms
  pretty HsIf {} = undefined
  pretty HsMultiIf {} = undefined
  pretty HsLet {} = undefined
  pretty (HsDo _ (DoExpr _) xs) = do
    string " do"
    newline
    indentedBlock $ inter newline $ output <$> unLoc xs
  -- While the name contains "Monad", this branch seems to be for list comprehensions.
  pretty (HsDo r MonadComp xs) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        string "["
        pretty $ last $ unLoc xs
        string " | "
        mapM_ pretty $ init $ unLoc xs
        string "]"
      vertical =
        insideVerticalList $
        case firstStmtAndOthers stmts of
          Just (lastStmt, others) -> do
            newline
            indentedBlock $ do
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
        string $ name' ++ " "
        indentedBlock $ pretty fields
      vertical = do
        newline
        indentedBlock $ do
          string name'
          (string " " >> pretty fields) `ifFitsOnOneLineOrElse`
            (newline >> indentedBlock (pretty fields))
      name' =
        if head (showOutputable name) == ':'
          then "(" ++ showOutputable name ++ ")"
          else showOutputable name
  pretty full@RecordUpd {} = output full
  pretty HsGetField {} = undefined
  pretty HsProjection {} = undefined
  pretty ExprWithTySig {} = undefined
  pretty ArithSeq {} = undefined
  pretty HsBracket {} = undefined
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
    if isInsideCase
      then do
        forM_ m_pats $ \x -> do
          output x
          string " -> "
        pretty m_grhss
      else do
        pretty m_ctxt
        unless (null m_pats) $
          forM_ m_pats $ \x -> do
            string " "
            output x
        (string " = " >> pretty m_grhss) `ifFitsOnOneLineOrElse`
          (string " =" >> pretty m_grhss)

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
  pretty TransStmt {} = undefined
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
    string " "
    pretty r
  pretty HsAppKindTy {} = undefined
  pretty full@HsFunTy {} = output full
  pretty HsListTy {} = undefined
  pretty full@HsTupleTy {} = output full
  pretty HsSumTy {} = undefined
  pretty (HsOpTy _ l op r) = do
    pretty l
    string " "
    insideSig <- gets psInsideSignature
    when insideSig $ string "'"
    pretty op
    string " "
    pretty r
  pretty (HsParTy _ inside) = do
    string "("
    pretty inside
    string ")"
  pretty HsIParamTy {} = undefined
  pretty HsStarTy {} = undefined
  pretty HsKindSig {} = undefined
  pretty HsSpliceTy {} = undefined
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
  pretty = output

instance Pretty (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty (GRHS _ _ body) = pretty body

instance Pretty EpaCommentTok where
  pretty (EpaLineComment c)  = string c
  pretty (EpaBlockComment c) = string c
  pretty _                   = return ()
