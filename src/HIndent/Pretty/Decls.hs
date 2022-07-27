{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Pretty.Decls
  ( outputDecls
  , declsExist
  ) where

import           Control.Monad
import           Data.Maybe
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
outputHsDecl (SigD _ s)     = outputSig s
outputHsDecl x              = outputOutputable x

outputTyClDecl :: TyClDecl GhcPs -> Printer ()
outputTyClDecl DataDecl {..} = do
  string "data "
  outputOutputable tcdLName
  outputHsDataDefn tcdDataDefn
outputTyClDecl x = outputOutputable x

outputInstDecl :: InstDecl GhcPs -> Printer ()
outputInstDecl ClsInstD {..} = outputClsInstDecl cid_inst
outputInstDecl x             = outputOutputable x

outputHsDataDefn :: HsDataDefn GhcPs -> Printer ()
outputHsDataDefn HsDataDefn {..} = do
  whenJust dd_kindSig $ \x -> do
    string " :: "
    outputOutputable x
  string " where"
  indentedBlock $
    forM_ dd_cons $ \x -> do
      newline
      outputConDecl $ unLoc x

outputConDecl :: ConDecl GhcPs -> Printer ()
outputConDecl ConDeclGADT {..} = horizontal `ifFitsOnOneLineOrElse` vertical
  where
    horizontal = do
      outputOutputable $ head con_names
      string " :: "
      outputHsConDeclGADTDetails con_g_args
      string " -> "
      outputOutputable con_res_ty
    vertical = do
      outputOutputable $ head con_names
      newline
      indentedBlock $ do
        indentedDependingOnHead (string ":: ") $
          outputHsConDeclGADTDetails con_g_args
        newline
        string "-> "
        outputOutputable con_res_ty
outputConDecl x = outputOutputable x

outputHsConDeclGADTDetails :: HsConDeclGADTDetails GhcPs -> Printer ()
outputHsConDeclGADTDetails (PrefixConGADT xs) =
  inter (string " -> ") $
  flip fmap xs $ \case
    (HsScaled _ x) -> outputOutputable x
outputHsConDeclGADTDetails (RecConGADT xs) = do
  string "{ "
  inter (newline >> string ", ") $
    flip fmap (unLoc xs) $ \(L _ ConDeclField {..}) -> do
      outputOutputable $ head cd_fld_names
      string " :: "
      outputOutputable cd_fld_type
  string "}"

outputClsInstDecl :: ClsInstDecl GhcPs -> Printer ()
outputClsInstDecl ClsInstDecl {..} = do
  string "instance "
  outputOutputable cid_poly_ty
  unless (isEmptyBag cid_binds) $ do
    string " where"
    newline
    indentedBlock $ mapM_ (outputHsBind . unLoc) cid_binds

outputHsBind :: HsBind GhcPs -> Printer ()
outputHsBind FunBind {..} = outputMatchGroup fun_matches
outputHsBind x            = outputOutputable x

outputMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> Printer ()
outputMatchGroup MG {..} = mapM_ (outputMatch . unLoc) $ unLoc mg_alts

outputMatch :: Match GhcPs (LHsExpr GhcPs) -> Printer ()
outputMatch Match {..} = do
  outputHsMatchContext m_ctxt
  unless (null m_pats) $
    forM_ m_pats $ \x -> do
      string " "
      outputOutputable x
  string " ="
  outputGRHSs m_grhss

outputHsMatchContext :: HsMatchContext GhcPs -> Printer ()
outputHsMatchContext FunRhs {..} = outputOutputable mc_fun
outputHsMatchContext x           = outputOutputable x

outputGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> Printer ()
outputGRHSs GRHSs {..} = mapM_ (outputGRHS . unLoc) grhssGRHSs

outputGRHS :: GRHS GhcPs (LHsExpr GhcPs) -> Printer ()
outputGRHS (GRHS _ _ body) = outputHsExpr $ unLoc body

outputHsExpr :: HsExpr GhcPs -> Printer ()
outputHsExpr (HsDo _ (DoExpr _) xs) = do
  string " do"
  newline
  indentedBlock $ inter newline $ outputOutputable <$> unLoc xs
-- While the name contains "Monad", this branch seems to be for list comprehensions.
outputHsExpr full@(HsDo r MonadComp xs) = do
  outputOutputable $ comments r
  (string " " >> outputOutputable full) `ifFitsOnOneLineOrElse` do
    newline
    indentedBlock $ do
      string "[ "
      outputStmtLR $ unLoc $ last $ unLoc xs
      newline
      string "| "
      inter (newline >> string ", ") $
        fmap (outputStmtLR . unLoc) $ init $ unLoc xs
      newline
      string "]"
outputHsExpr x = do
  string " "
  outputOutputable x

outputStmtLR :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Printer ()
outputStmtLR full@(BindStmt _ pat body) =
  outputOutputable full `ifFitsOnOneLineOrElse` do
    outputOutputable pat
    string " <-"
    newline
    indentedBlock $ indentedWithSpace 2 $ outputOutputable body -- 2 for "| "
outputStmtLR x = outputOutputable x

outputSig :: Sig GhcPs -> Printer ()
outputSig (TypeSig _ funName params) =
  outputTypeSig (unLoc $ head funName) (unLoc $ hswc_body params)
outputSig x = outputOutputable x

outputTypeSig :: IdP GhcPs -> HsSigType GhcPs -> Printer ()
outputTypeSig funName params = do
  outputOutputable funName
  string " :: "
  outputSigType params

outputSigType :: HsSigType GhcPs -> Printer ()
outputSigType HsSig {..} = outputHsType $ unLoc sig_body

outputHsType :: HsType GhcPs -> Printer ()
outputHsType HsForAllTy {} = undefined
outputHsType HsQualTy {} = undefined
outputHsType x@HsTyVar {} = outputOutputable x
outputHsType (HsAppTy _ l r) = do
  outputHsType $ unLoc l
  string " "
  outputHsType $ unLoc r
outputHsType HsAppKindTy {} = undefined
outputHsType HsFunTy {} = undefined
outputHsType HsListTy {} = undefined
outputHsType HsTupleTy {} = undefined
outputHsType HsSumTy {} = undefined
outputHsType (HsOpTy _ l op r) = do
  outputHsType $ unLoc l
  string " "
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
outputHsType x = outputOutputable x

outputRdrName :: RdrName -> Printer ()
outputRdrName = outputOutputable

addSeparator :: [HsDecl GhcPs] -> [(HsDecl GhcPs, Maybe (Printer ()))]
addSeparator []     = []
addSeparator [x]    = [(x, Nothing)]
addSeparator (x:xs) = (x, Just $ separator x) : addSeparator xs

separator :: HsDecl GhcPs -> Printer ()
separator SigD {} = newline
separator _       = blankline
