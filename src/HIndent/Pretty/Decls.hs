{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Pretty.Decls
  ( outputDecls
  , declsExist
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
import           HIndent.Types

outputDecls :: HsModule -> Printer ()
outputDecls =
  mapM_ (\(x, sp) -> outputHsDecl x >> fromMaybe (return ()) sp) .
  addSeparator . fmap unLoc . hsmodDecls

declsExist :: HsModule -> Bool
declsExist = not . null . hsmodDecls

outputHsDecl :: HsDecl GhcPs -> Printer ()
outputHsDecl (TyClD _ d)    = outputTyClDecl d
outputHsDecl (InstD _ inst) = outputInstDecl inst
outputHsDecl (ValD _ bind)  = outputHsBind bind
outputHsDecl (SigD _ s)     = insideSignature $ outputSig s
outputHsDecl x              = output x

outputTyClDecl :: TyClDecl GhcPs -> Printer ()
outputTyClDecl DataDecl {..} = do
  string "data "
  output tcdLName
  outputHsDataDefn tcdDataDefn
outputTyClDecl x = output x

outputInstDecl :: InstDecl GhcPs -> Printer ()
outputInstDecl ClsInstD {..} = outputClsInstDecl cid_inst
outputInstDecl x             = output x

outputHsDataDefn :: HsDataDefn GhcPs -> Printer ()
outputHsDataDefn HsDataDefn {..} = do
  whenJust dd_kindSig $ \x -> do
    string " :: "
    output x
  string " where"
  indentedBlock $
    forM_ dd_cons $ \x -> do
      newline
      outputConDecl $ unLoc x

outputConDecl :: ConDecl GhcPs -> Printer ()
outputConDecl ConDeclGADT {..} = horizontal `ifFitsOnOneLineOrElse` vertical
  where
    horizontal = do
      output $ head con_names
      string " :: "
      outputHsConDeclGADTDetails con_g_args
      string " -> "
      output con_res_ty
    vertical = do
      output $ head con_names
      newline
      indentedBlock $ do
        indentedDependingOnHead (string ":: ") $
          outputHsConDeclGADTDetails con_g_args
        newline
        string "-> "
        output con_res_ty
outputConDecl x = output x

outputHsConDeclGADTDetails :: HsConDeclGADTDetails GhcPs -> Printer ()
outputHsConDeclGADTDetails (PrefixConGADT xs) =
  inter (string " -> ") $
  flip fmap xs $ \case
    (HsScaled _ x) -> output x
outputHsConDeclGADTDetails (RecConGADT xs) = do
  string "{ "
  inter (newline >> string ", ") $
    flip fmap (unLoc xs) $ \(L _ ConDeclField {..}) -> do
      output $ head cd_fld_names
      string " :: "
      output cd_fld_type
  string "}"

outputClsInstDecl :: ClsInstDecl GhcPs -> Printer ()
outputClsInstDecl ClsInstDecl {..} = do
  string "instance "
  output cid_poly_ty
  unless (isEmptyBag cid_binds) $ do
    string " where"
    newline
    indentedBlock $ mapM_ (outputHsBind . unLoc) cid_binds

outputHsBind :: HsBind GhcPs -> Printer ()
outputHsBind FunBind {..} = outputMatchGroup fun_matches
outputHsBind x            = output x

outputMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> Printer ()
outputMatchGroup MG {..} = do
  isInsideCase <- gets psInsideCase
  inter (when isInsideCase newline) $ outputMatch . unLoc <$> unLoc mg_alts

outputMatch :: Match GhcPs (LHsExpr GhcPs) -> Printer ()
outputMatch Match {..} = do
  isInsideCase <- gets psInsideCase
  if isInsideCase
    then do
      forM_ m_pats $ \x -> do
        output x
        string " -> "
      outputGRHSs m_grhss
    else do
      outputHsMatchContext m_ctxt
      unless (null m_pats) $
        forM_ m_pats $ \x -> do
          string " "
          output x
      (string " = " >> outputGRHSs m_grhss) `ifFitsOnOneLineOrElse`
        (string " =" >> outputGRHSs m_grhss)

outputHsMatchContext :: HsMatchContext GhcPs -> Printer ()
outputHsMatchContext FunRhs {..} = output mc_fun
outputHsMatchContext CaseAlt     = return ()
outputHsMatchContext x           = output x

outputGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> Printer ()
outputGRHSs GRHSs {..} = mapM_ (outputGRHS . unLoc) grhssGRHSs

outputGRHS :: GRHS GhcPs (LHsExpr GhcPs) -> Printer ()
outputGRHS (GRHS _ _ body) = outputHsExpr $ unLoc body

data StmtOrComment
  = Stmt (LStmtLR GhcPs GhcPs (LHsExpr GhcPs))
  | Comment LEpaComment

sortByLocation :: [StmtOrComment] -> [StmtOrComment]
sortByLocation = sortBy (compare `on` getLocation)

getLocation :: StmtOrComment -> RealSrcSpan
getLocation (Stmt x)    = realSrcSpan $ locA $ getLoc x
getLocation (Comment x) = anchor $ getLoc x

outputStmtOrComment :: StmtOrComment -> Printer ()
outputStmtOrComment (Stmt x)    = outputStmtLR $ unLoc x
outputStmtOrComment (Comment x) = printComment $ ac_tok $ unLoc x

outputHsExpr :: HsExpr GhcPs -> Printer ()
outputHsExpr (HsVar _ v) = output v
outputHsExpr HsUnboundVar {} = undefined
outputHsExpr HsConLikeOut {} = undefined
outputHsExpr HsRecFld {} = undefined
outputHsExpr HsOverLabel {} = undefined
outputHsExpr HsIPVar {} = undefined
outputHsExpr full@HsOverLit {} = output full
outputHsExpr (HsLit _ l) = output l
outputHsExpr full@HsLam {} = output full
outputHsExpr HsLamCase {} = undefined
outputHsExpr full@HsApp {} = output full
outputHsExpr HsAppType {} = undefined
outputHsExpr full@(OpApp _ l o r) =
  output full `ifFitsOnOneLineOrElse` do
    newline
    indentedBlock $ do
      outputHsExpr $ unLoc l
      string " "
      outputHsExpr $ unLoc o
      newline
      outputHsExpr $ unLoc r
outputHsExpr NegApp {} = undefined
outputHsExpr HsPar {} = undefined
outputHsExpr SectionL {} = undefined
outputHsExpr SectionR {} = undefined
outputHsExpr ExplicitTuple {} = undefined
outputHsExpr ExplicitSum {} = undefined
outputHsExpr (HsCase _ cond arms) =
  insideCase $ do
    newline
    indentedBlock $ do
      string "case "
      output cond
      string " of"
      newline
      indentedBlock $ outputMatchGroup arms
outputHsExpr HsIf {} = undefined
outputHsExpr HsMultiIf {} = undefined
outputHsExpr HsLet {} = undefined
outputHsExpr (HsDo _ (DoExpr _) xs) = do
  string " do"
  newline
  indentedBlock $ inter newline $ output <$> unLoc xs
-- While the name contains "Monad", this branch seems to be for list comprehensions.
outputHsExpr (HsDo r MonadComp xs) = horizontal `ifFitsOnOneLineOrElse` vertical
  where
    horizontal = do
      string "["
      outputStmtLR $ unLoc $ last $ unLoc xs
      string " | "
      mapM_ (outputStmtLR . unLoc) $ init $ unLoc xs
      string "]"
    vertical =
      insideVerticalList $
      case firstStmtAndOthers stmts of
        Just (lastStmt, others) -> do
          newline
          indentedBlock $ do
            string "[ "
            outputStmtLR $ unLoc lastStmt
            newline
            forM_ (stmtsAndPrefixes others) $ \(p, x) -> do
              string p
              outputStmtOrComment x
              newline
            string "]"
        Nothing -> string "[]"
    stmtsAndPrefixes l = ("| ", head l) : fmap (\x -> (prefix x, x)) (tail l)
    prefix Stmt {}    = ", "
    prefix Comment {} = "  "
    stmts =
      sortByLocation $
      fmap Comment (listify (const True) r) ++ fmap Stmt (unLoc xs)
outputHsExpr HsDo {} = undefined
outputHsExpr ExplicitList {} = undefined
outputHsExpr (RecordCon _ name fields) =
  horizontal `ifFitsOnOneLineOrElse` vertical
  where
    horizontal = do
      string $ name' ++ " "
      indentedBlock $ outputHsRecordBinds fields
    vertical = do
      newline
      indentedBlock $ do
        string name'
        (string " " >> outputHsRecordBinds fields) `ifFitsOnOneLineOrElse`
          (newline >> indentedBlock (outputHsRecordBinds fields))
    name' =
      if head (showOutputable name) == ':'
        then "(" ++ showOutputable name ++ ")"
        else showOutputable name
outputHsExpr full@RecordUpd {} = output full
outputHsExpr HsGetField {} = undefined
outputHsExpr HsProjection {} = undefined
outputHsExpr ExprWithTySig {} = undefined
outputHsExpr ArithSeq {} = undefined
outputHsExpr HsBracket {} = undefined
outputHsExpr HsRnBracketOut {} = undefined
outputHsExpr HsTcBracketOut {} = undefined
outputHsExpr HsSpliceE {} = undefined
outputHsExpr HsProc {} = undefined
outputHsExpr HsStatic {} = undefined
outputHsExpr HsTick {} = undefined
outputHsExpr HsBinTick {} = undefined
outputHsExpr HsPragE {} = undefined

outputHsRecordBinds :: HsRecordBinds GhcPs -> Printer ()
outputHsRecordBinds HsRecFields {..} =
  horizontal `ifFitsOnOneLineOrElse` vertical
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

firstStmtAndOthers ::
     [StmtOrComment]
  -> Maybe (LStmtLR GhcPs GhcPs (LHsExpr GhcPs), [StmtOrComment])
firstStmtAndOthers = f []
  where
    f _ []                 = Nothing
    f xs (Stmt y:ys)       = Just (y, xs ++ ys)
    f xs (y@Comment {}:ys) = f (y : xs) ys

outputStmtLR :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Printer ()
outputStmtLR l@LastStmt {} = output l
outputStmtLR full@(BindStmt _ pat body) =
  output full `ifFitsOnOneLineOrElse` do
    output pat
    string " <-"
    newline
    indentedBlock $ indentedWithSpace 2 $ output body -- 2 for "| "
outputStmtLR ApplicativeStmt {} = undefined
outputStmtLR BodyStmt {} = undefined
outputStmtLR l@LetStmt {} = output l
outputStmtLR (ParStmt _ xs _ _) = do
  inVertical <- gets psInsideVerticalList
  if inVertical
    then vertical
    else horizontal `ifFitsOnOneLineOrElse` vertical
  where
    horizontal = inter (string " | ") $ fmap output xs
    vertical = inter (newline >> string "| ") $ fmap outputParStmtBlock xs
outputStmtLR TransStmt {} = undefined
outputStmtLR RecStmt {} = undefined

outputParStmtBlock :: ParStmtBlock GhcPs GhcPs -> Printer ()
outputParStmtBlock (ParStmtBlock _ xs _ _) = do
  inVertical <- gets psInsideVerticalList
  if inVertical
    then vertical
    else horizontal `ifFitsOnOneLineOrElse` vertical
  where
    horizontal = inter (string ", ") $ fmap output xs
    vertical = inter (newline >> string ", ") $ fmap output xs

outputSig :: Sig GhcPs -> Printer ()
outputSig (TypeSig _ funName params) =
  outputTypeSig (unLoc $ head funName) (unLoc $ hswc_body params)
outputSig x = output x

outputTypeSig :: IdP GhcPs -> HsSigType GhcPs -> Printer ()
outputTypeSig funName params = do
  output funName
  string " :: "
  outputSigType params

outputSigType :: HsSigType GhcPs -> Printer ()
outputSigType HsSig {..} = outputHsType $ unLoc sig_body

outputHsType :: HsType GhcPs -> Printer ()
outputHsType HsForAllTy {} = undefined
outputHsType HsQualTy {} = undefined
outputHsType x@HsTyVar {} = output x
outputHsType (HsAppTy _ l r) = do
  outputHsType $ unLoc l
  string " "
  outputHsType $ unLoc r
outputHsType HsAppKindTy {} = undefined
outputHsType full@HsFunTy {} = output full
outputHsType HsListTy {} = undefined
outputHsType full@HsTupleTy {} = output full
outputHsType HsSumTy {} = undefined
outputHsType (HsOpTy _ l op r) = do
  outputHsType $ unLoc l
  string " "
  insideSig <- gets psInsideSignature
  when insideSig $ string "'"
  outputRdrName $ unLoc op
  string " "
  outputHsType $ unLoc r
outputHsType (HsParTy _ inside) = do
  string "("
  outputHsType $ unLoc inside
  string ")"
outputHsType HsIParamTy {} = undefined
outputHsType HsStarTy {} = undefined
outputHsType HsKindSig {} = undefined
outputHsType HsSpliceTy {} = undefined
outputHsType HsDocTy {} = undefined
outputHsType HsBangTy {} = undefined
outputHsType HsRecTy {} = undefined
outputHsType (HsExplicitListTy _ _ xs) = do
  string "'[ "
  inter (string ", ") $ fmap (outputHsType . unLoc) xs
  string "]"
outputHsType HsExplicitTupleTy {} = undefined
outputHsType HsTyLit {} = undefined
outputHsType HsWildCardTy {} = undefined
outputHsType XHsType {} = undefined

outputRdrName :: RdrName -> Printer ()
outputRdrName = output

addSeparator :: [HsDecl GhcPs] -> [(HsDecl GhcPs, Maybe (Printer ()))]
addSeparator []     = []
addSeparator [x]    = [(x, Nothing)]
addSeparator (x:xs) = (x, Just $ separator x) : addSeparator xs

separator :: HsDecl GhcPs -> Printer ()
separator SigD {} = newline
separator _       = blankline
