{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- | Comment handling around an AST node
module HIndent.Pretty.NodeComments
  ( CommentExtraction(..)
  , emptyNodeComments
  ) where

import Data.Maybe
import Data.Void
import GHC.Core.Coercion
import GHC.Data.BooleanFormula
import GHC.Hs
import GHC.Stack
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import HIndent.Ast.NodeComments
import HIndent.Pretty.SigBindFamily
import HIndent.Pretty.Types
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
import GHC.Core.DataCon
#else
import GHC.Unit
#endif
-- | An interface to extract comments from an AST node.
class CommentExtraction a where
  nodeComments :: a -> NodeComments

instance CommentExtraction l => CommentExtraction (GenLocated l e) where
  nodeComments (L l _) = nodeComments l

instance CommentExtraction (MatchGroup GhcPs a) where
  nodeComments MG {} = emptyNodeComments

instance CommentExtraction LambdaCase where
  nodeComments (LambdaCase x _) = nodeComments x

instance CommentExtraction DoOrMdo where
  nodeComments = const emptyNodeComments

instance CommentExtraction QualifiedDo where
  nodeComments = const emptyNodeComments

instance CommentExtraction (HsSigType GhcPs) where
  nodeComments HsSig {} = emptyNodeComments

instance CommentExtraction HsSigType' where
  nodeComments (HsSigType' _ _ HsSig {}) = emptyNodeComments

instance CommentExtraction StmtLRInsideVerticalList where
  nodeComments (StmtLRInsideVerticalList x) = nodeComments x

-- | For pattern matching.
instance CommentExtraction
           (HsRecFields GhcPs (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  nodeComments HsRecFields {} = emptyNodeComments

-- | For record updates
instance CommentExtraction
           (HsRecFields GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  nodeComments HsRecFields {} = emptyNodeComments

instance CommentExtraction HsType' where
  nodeComments HsType' {..} = nodeComments hsType

instance CommentExtraction (GRHSs GhcPs a) where
  nodeComments GRHSs {..} = NodeComments {..}
    where
      commentsBefore = priorComments grhssExt
      commentsOnSameLine = []
      commentsAfter = getFollowingComments grhssExt

instance CommentExtraction GRHSsExpr where
  nodeComments (GRHSsExpr {..}) = nodeComments grhssExpr

instance CommentExtraction (ParStmtBlock GhcPs GhcPs) where
  nodeComments ParStmtBlock {} = emptyNodeComments

instance CommentExtraction ParStmtBlockInsideVerticalList where
  nodeComments (ParStmtBlockInsideVerticalList x) = nodeComments x

instance CommentExtraction RdrName where
  nodeComments Unqual {} = emptyNodeComments
  nodeComments Qual {} = emptyNodeComments
  nodeComments Orig {} = emptyNodeComments
  nodeComments Exact {} = emptyNodeComments

instance CommentExtraction (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  nodeComments = nodeComments . GRHSExpr GRHSExprNormal

instance CommentExtraction GRHSExpr where
  nodeComments (GRHSExpr {grhsExpr = (GRHS x _ _)}) = nodeComments x

instance CommentExtraction GRHSProc where
  nodeComments (GRHSProc (GRHS x _ _)) = nodeComments x

instance CommentExtraction EpaCommentTok where
  nodeComments = const emptyNodeComments

instance CommentExtraction (SpliceDecl GhcPs) where
  nodeComments SpliceDecl {} = emptyNodeComments

instance CommentExtraction PatInsidePatDecl where
  nodeComments (PatInsidePatDecl x) = nodeComments x

instance CommentExtraction RecConPat where
  nodeComments (RecConPat x) = nodeComments x

instance CommentExtraction SigBindFamily where
  nodeComments (Sig x) = nodeComments x
  nodeComments (Bind x) = nodeComments x
  nodeComments (TypeFamily x) = nodeComments x
  nodeComments (TyFamInst x) = nodeComments x
  nodeComments (DataFamInst x) = nodeComments x

instance CommentExtraction EpaComment where
  nodeComments EpaComment {} = emptyNodeComments

instance CommentExtraction SrcSpan where
  nodeComments RealSrcSpan {} = emptyNodeComments
  nodeComments UnhelpfulSpan {} = emptyNodeComments

instance CommentExtraction (HsLocalBindsLR GhcPs GhcPs) where
  nodeComments (HsValBinds x _) = nodeComments x
  nodeComments (HsIPBinds x _) = nodeComments x
  nodeComments EmptyLocalBinds {} = emptyNodeComments

instance CommentExtraction (HsValBindsLR GhcPs GhcPs) where
  nodeComments ValBinds {} = emptyNodeComments
  nodeComments XValBindsLR {} = notUsedInParsedStage

instance CommentExtraction (FieldOcc GhcPs) where
  nodeComments FieldOcc {} = emptyNodeComments

-- HsConDeclH98Details
instance CommentExtraction
           (HsConDetails
              Void
              (HsScaled GhcPs (GenLocated SrcSpanAnnA (BangType GhcPs)))
              (GenLocated
                 SrcSpanAnnL
                 [GenLocated SrcSpanAnnA (ConDeclField GhcPs)])) where
  nodeComments PrefixCon {} = emptyNodeComments
  nodeComments RecCon {} = emptyNodeComments
  nodeComments InfixCon {} = emptyNodeComments

instance CommentExtraction (HsScaled GhcPs a) where
  nodeComments HsScaled {} = emptyNodeComments

instance CommentExtraction InfixExpr where
  nodeComments (InfixExpr x) = nodeComments x

instance CommentExtraction InfixApp where
  nodeComments InfixApp {} = emptyNodeComments

instance CommentExtraction (BooleanFormula a) where
  nodeComments Var {} = emptyNodeComments
  nodeComments And {} = emptyNodeComments
  nodeComments Or {} = emptyNodeComments
  nodeComments Parens {} = emptyNodeComments

instance CommentExtraction (FieldLabelStrings GhcPs) where
  nodeComments FieldLabelStrings {} = emptyNodeComments

instance CommentExtraction (AmbiguousFieldOcc GhcPs) where
  nodeComments Unambiguous {} = emptyNodeComments
  nodeComments Ambiguous {} = emptyNodeComments

instance CommentExtraction (ImportDecl GhcPs) where
  nodeComments ImportDecl {..} = nodeComments ideclExt

instance CommentExtraction (DerivClauseTys GhcPs) where
  nodeComments DctSingle {} = emptyNodeComments
  nodeComments DctMulti {} = emptyNodeComments

instance CommentExtraction OverlapMode where
  nodeComments NoOverlap {} = emptyNodeComments
  nodeComments Overlappable {} = emptyNodeComments
  nodeComments Overlapping {} = emptyNodeComments
  nodeComments Overlaps {} = emptyNodeComments
  nodeComments Incoherent {} = emptyNodeComments

instance CommentExtraction StringLiteral where
  nodeComments StringLiteral {} = emptyNodeComments

instance CommentExtraction (FamilyResultSig GhcPs) where
  nodeComments NoSig {} = emptyNodeComments
  nodeComments KindSig {} = emptyNodeComments
  nodeComments TyVarSig {} = emptyNodeComments

instance CommentExtraction (ArithSeqInfo GhcPs) where
  nodeComments From {} = emptyNodeComments
  nodeComments FromThen {} = emptyNodeComments
  nodeComments FromTo {} = emptyNodeComments
  nodeComments FromThenTo {} = emptyNodeComments

instance CommentExtraction (HsForAllTelescope GhcPs) where
  nodeComments HsForAllVis {..} = nodeComments hsf_xvis
  nodeComments HsForAllInvis {..} = nodeComments hsf_xinvis

instance CommentExtraction InfixOp where
  nodeComments (InfixOp x) = nodeComments x

instance CommentExtraction PrefixOp where
  nodeComments (PrefixOp x) = nodeComments x

instance CommentExtraction Context where
  nodeComments Context {} = emptyNodeComments

instance CommentExtraction HorizontalContext where
  nodeComments HorizontalContext {} = emptyNodeComments

instance CommentExtraction VerticalContext where
  nodeComments VerticalContext {} = emptyNodeComments

-- Wrap a value of this type with 'ModulenameWithPrefix' to print it with
-- the "module " prefix.
instance CommentExtraction ModuleName where
  nodeComments = const emptyNodeComments

instance CommentExtraction ModuleNameWithPrefix where
  nodeComments ModuleNameWithPrefix {} = emptyNodeComments

instance CommentExtraction FamEqn' where
  nodeComments FamEqn' {..} = nodeComments famEqn

instance CommentExtraction (IEWrappedName a) where
  nodeComments IEName {} = emptyNodeComments
  nodeComments IEPattern {} = emptyNodeComments
  nodeComments IEType {} = emptyNodeComments

instance CommentExtraction OccName where
  nodeComments = const emptyNodeComments

-- | 'Pretty' for 'LHsSigWcType GhcPs'.
instance CommentExtraction
           (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsSigType GhcPs))) where
  nodeComments HsWC {} = emptyNodeComments

-- | 'Pretty' for 'LHsWcType'
instance CommentExtraction
           (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  nodeComments HsWC {} = emptyNodeComments

instance CommentExtraction CExportSpec where
  nodeComments CExportStatic {} = emptyNodeComments

instance CommentExtraction Safety where
  nodeComments PlaySafe = emptyNodeComments
  nodeComments PlayInterruptible = emptyNodeComments
  nodeComments PlayRisky = emptyNodeComments

instance CommentExtraction Role where
  nodeComments Nominal = emptyNodeComments
  nodeComments Representational = emptyNodeComments
  nodeComments Phantom = emptyNodeComments

instance CommentExtraction TopLevelTyFamInstDecl where
  nodeComments (TopLevelTyFamInstDecl x) = nodeComments x

instance CommentExtraction (DataFamInstDecl GhcPs) where
  nodeComments DataFamInstDecl {} = emptyNodeComments

instance CommentExtraction DataFamInstDecl' where
  nodeComments DataFamInstDecl' {..} = nodeComments dataFamInstDecl

-- | 'Pretty' for 'HsPatSynDetails'.
instance CommentExtraction
           (HsConDetails
              Void
              (GenLocated SrcSpanAnnN RdrName)
              [RecordPatSynField GhcPs]) where
  nodeComments PrefixCon {} = emptyNodeComments
  nodeComments RecCon {} = emptyNodeComments
  nodeComments InfixCon {} = emptyNodeComments

instance CommentExtraction (FixitySig GhcPs) where
  nodeComments FixitySig {} = emptyNodeComments

instance CommentExtraction Fixity where
  nodeComments Fixity {} = emptyNodeComments

instance CommentExtraction FixityDirection where
  nodeComments InfixL {} = emptyNodeComments
  nodeComments InfixR {} = emptyNodeComments
  nodeComments InfixN {} = emptyNodeComments

instance CommentExtraction InlinePragma where
  nodeComments InlinePragma {} = emptyNodeComments

instance CommentExtraction (HsPatSynDir GhcPs) where
  nodeComments Unidirectional = emptyNodeComments
  nodeComments ImplicitBidirectional = emptyNodeComments
  nodeComments ExplicitBidirectional {} = emptyNodeComments

instance CommentExtraction (HsOverLit GhcPs) where
  nodeComments OverLit {} = emptyNodeComments

instance CommentExtraction OverLitVal where
  nodeComments HsIntegral {} = emptyNodeComments
  nodeComments HsFractional {} = emptyNodeComments
  nodeComments HsIsString {} = emptyNodeComments

instance CommentExtraction IntegralLit where
  nodeComments IL {} = emptyNodeComments

instance CommentExtraction FractionalLit where
  nodeComments FL {} = emptyNodeComments

instance CommentExtraction (HsLit GhcPs) where
  nodeComments HsChar {} = emptyNodeComments
  nodeComments HsCharPrim {} = emptyNodeComments
  nodeComments HsString {} = emptyNodeComments
  nodeComments HsStringPrim {} = emptyNodeComments
  nodeComments HsInt {} = emptyNodeComments
  nodeComments HsIntPrim {} = emptyNodeComments
  nodeComments HsWordPrim {} = emptyNodeComments
  nodeComments HsInt64Prim {} = emptyNodeComments
  nodeComments HsWord64Prim {} = emptyNodeComments
  nodeComments HsInteger {} = emptyNodeComments
  nodeComments HsRat {} = emptyNodeComments
  nodeComments HsFloatPrim {} = emptyNodeComments
  nodeComments HsDoublePrim {} = emptyNodeComments

instance CommentExtraction HsIPName where
  nodeComments HsIPName {} = emptyNodeComments

instance CommentExtraction (HsPatSigType GhcPs) where
  nodeComments HsPS {..} = nodeComments hsps_ext

instance CommentExtraction (HsIPBinds GhcPs) where
  nodeComments IPBinds {} = emptyNodeComments

instance CommentExtraction (RecordPatSynField GhcPs) where
  nodeComments RecordPatSynField {} = emptyNodeComments

instance CommentExtraction (HsCmdTop GhcPs) where
  nodeComments HsCmdTop {} = emptyNodeComments

instance CommentExtraction ListComprehension where
  nodeComments ListComprehension {} = emptyNodeComments

instance CommentExtraction DoExpression where
  nodeComments DoExpression {} = emptyNodeComments

instance CommentExtraction LetIn where
  nodeComments LetIn {} = emptyNodeComments

instance CommentExtraction CCallConv where
  nodeComments = const emptyNodeComments

instance CommentExtraction HsSrcBang where
  nodeComments HsSrcBang {} = emptyNodeComments

instance CommentExtraction SrcUnpackedness where
  nodeComments SrcUnpack = emptyNodeComments
  nodeComments SrcNoUnpack = emptyNodeComments
  nodeComments NoSrcUnpack = emptyNodeComments

instance CommentExtraction SrcStrictness where
  nodeComments SrcLazy = emptyNodeComments
  nodeComments SrcStrict = emptyNodeComments
  nodeComments NoSrcStrict = emptyNodeComments

instance CommentExtraction (HsOuterSigTyVarBndrs GhcPs) where
  nodeComments HsOuterImplicit {} = emptyNodeComments
  nodeComments HsOuterExplicit {..} = nodeComments hso_xexplicit

instance CommentExtraction AddEpAnn where
  nodeComments (AddEpAnn _ x) = nodeComments x

instance CommentExtraction EpaLocation where
  nodeComments EpaSpan {} = emptyNodeComments
  nodeComments (EpaDelta _ x) = mconcat $ fmap nodeComments x

instance CommentExtraction AnnPragma where
  nodeComments AnnPragma {..} =
    mconcat $ fmap nodeComments $ apr_open : apr_close : apr_rest

instance CommentExtraction HsRuleAnn where
  nodeComments HsRuleAnn {..} =
    mconcat $ f ra_tyanns : f ra_tmanns : fmap nodeComments ra_rest
    where
      f (Just (x, y)) = mconcat $ fmap nodeComments [x, y]
      f Nothing = emptyNodeComments

instance CommentExtraction AnnFieldLabel where
  nodeComments AnnFieldLabel {afDot = Just x} = nodeComments x
  nodeComments AnnFieldLabel {afDot = Nothing} = emptyNodeComments

instance CommentExtraction EpAnnSumPat where
  nodeComments EpAnnSumPat {..} =
    mconcat
      $ fmap nodeComments sumPatParens
          <> fmap nodeComments sumPatVbarsBefore
          <> fmap nodeComments sumPatVbarsAfter

instance CommentExtraction AnnProjection where
  nodeComments AnnProjection {..} =
    mconcat $ fmap nodeComments [apOpen, apClose]

instance CommentExtraction AnnsIf where
  nodeComments AnnsIf {..} =
    mconcat
      $ fmap nodeComments
      $ aiIf
          : aiThen
          : aiElse
          : (maybeToList aiThenSemi <> maybeToList aiElseSemi)

instance CommentExtraction EpAnnHsCase where
  nodeComments EpAnnHsCase {..} =
    mconcat
      $ nodeComments hsCaseAnnCase
          : nodeComments hsCaseAnnOf
          : fmap nodeComments hsCaseAnnsRest

instance CommentExtraction AnnExplicitSum where
  nodeComments AnnExplicitSum {..} =
    mconcat
      $ fmap nodeComments
      $ aesOpen : aesBarsBefore <> aesBarsAfter <> [aesClose]

instance CommentExtraction EpAnnUnboundVar where
  nodeComments EpAnnUnboundVar {..} =
    mconcat
      $ fmap
          nodeComments
          [fst hsUnboundBackquotes, snd hsUnboundBackquotes, hsUnboundHole]

instance CommentExtraction AnnSig where
  nodeComments AnnSig {..} = mconcat $ fmap nodeComments $ asDcolon : asRest

instance CommentExtraction (HsBind GhcPs) where
  nodeComments = nodeCommentsHsBind

nodeCommentsHsBind :: HsBind GhcPs -> NodeComments
nodeCommentsHsBind FunBind {..} = nodeComments fun_id
#if MIN_VERSION_ghc_lib_parser(9, 10, 0)
nodeCommentsHsBind PatBind {} = emptyNodeComments
#else
nodeCommentsHsBind PatBind {..} = nodeComments pat_ext
#endif
nodeCommentsHsBind VarBind {} = emptyNodeComments
#if !MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsBind AbsBinds {} = emptyNodeComments
#endif
nodeCommentsHsBind PatSynBind {} = emptyNodeComments

instance CommentExtraction (Sig GhcPs) where
  nodeComments = nodeCommentsSig

nodeCommentsSig :: Sig GhcPs -> NodeComments
nodeCommentsSig (TypeSig x _ _) = nodeComments x
nodeCommentsSig (PatSynSig x _ _) = nodeComments x
nodeCommentsSig (ClassOpSig x _ _ _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsSig (FixSig x _) = mconcat $ fmap nodeComments x
nodeCommentsSig (InlineSig x _ _) = mconcat $ fmap nodeComments x
nodeCommentsSig (SpecSig x _ _ _) = mconcat $ fmap nodeComments x
nodeCommentsSig (SpecInstSig (x, _) _) = mconcat $ fmap nodeComments x
nodeCommentsSig (MinimalSig (x, _) _) = mconcat $ fmap nodeComments x
nodeCommentsSig (SCCFunSig (x, _) _ _) = mconcat $ fmap nodeComments x
nodeCommentsSig (CompleteMatchSig (x, _) _ _) = mconcat $ fmap nodeComments x
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsSig (FixSig x _) = nodeComments x
nodeCommentsSig (InlineSig x _ _) = nodeComments x
nodeCommentsSig (SpecSig x _ _ _) = nodeComments x
nodeCommentsSig (SpecInstSig (x, _) _) = nodeComments x
nodeCommentsSig (MinimalSig (x, _) _) = nodeComments x
nodeCommentsSig (SCCFunSig (x, _) _ _) = nodeComments x
nodeCommentsSig (CompleteMatchSig (x, _) _ _) = nodeComments x
#else
nodeCommentsSig (FixSig x _) = nodeComments x
nodeCommentsSig (InlineSig x _ _) = nodeComments x
nodeCommentsSig (SpecSig x _ _ _) = nodeComments x
nodeCommentsSig IdSig {} = emptyNodeComments
nodeCommentsSig (SpecInstSig x _ _) = nodeComments x
nodeCommentsSig (MinimalSig x _ _) = nodeComments x
nodeCommentsSig (SCCFunSig x _ _ _) = nodeComments x
nodeCommentsSig (CompleteMatchSig x _ _ _) = nodeComments x
#endif
instance CommentExtraction (HsExpr GhcPs) where
  nodeComments = nodeCommentsHsExpr

nodeCommentsHsExpr :: HsExpr GhcPs -> NodeComments
nodeCommentsHsExpr HsVar {} = emptyNodeComments
nodeCommentsHsExpr HsLam {} = emptyNodeComments
nodeCommentsHsExpr HsAppType {} = emptyNodeComments
nodeCommentsHsExpr (ExplicitSum x _ _ _) = nodeComments x
nodeCommentsHsExpr (HsCase x _ _) = nodeComments x
nodeCommentsHsExpr (HsIf x _ _ _) = nodeComments x
nodeCommentsHsExpr (HsDo x _ _) = nodeComments x
nodeCommentsHsExpr (ExplicitList x _) = nodeComments x
nodeCommentsHsExpr HsProjection {..} = nodeComments proj_ext
nodeCommentsHsExpr HsPragE {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (HsTypedBracket x _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (HsUntypedBracket x _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr HsLet {} = emptyNodeComments
nodeCommentsHsExpr HsPar {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsExpr HsRecSel {} = emptyNodeComments
nodeCommentsHsExpr (HsTypedBracket x _) = nodeComments x
nodeCommentsHsExpr (HsUntypedBracket x _) = nodeComments x
nodeCommentsHsExpr (HsLet x _ _ _ _) = nodeComments x
nodeCommentsHsExpr (HsPar x _ _ _) = nodeComments x
nodeCommentsHsExpr (HsLamCase x _ _) = nodeComments x
#else
nodeCommentsHsExpr HsTick {} = emptyNodeComments
nodeCommentsHsExpr HsBinTick {} = emptyNodeComments
nodeCommentsHsExpr (HsBracket x _) = nodeComments x
nodeCommentsHsExpr HsRnBracketOut {} = notUsedInParsedStage
nodeCommentsHsExpr HsTcBracketOut {} = notUsedInParsedStage
nodeCommentsHsExpr (HsLet x _ _) = nodeComments x
nodeCommentsHsExpr (HsPar x _) = nodeComments x
nodeCommentsHsExpr (HsLamCase x _) = nodeComments x
nodeCommentsHsExpr HsConLikeOut {} = emptyNodeComments
nodeCommentsHsExpr HsRecFld {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (HsTypedSplice x _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr HsUntypedSplice {} = emptyNodeComments
nodeCommentsHsExpr HsOverLabel {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsHsExpr (HsTypedSplice (x, y) _) = nodeComments x <> nodeComments y
nodeCommentsHsExpr (HsUntypedSplice x _) = nodeComments x
nodeCommentsHsExpr (HsOverLabel x _ _) = nodeComments x
#else
nodeCommentsHsExpr (HsSpliceE x _) = nodeComments x
nodeCommentsHsExpr (HsOverLabel x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsExpr (HsStatic x _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (HsProc x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (ArithSeq x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (ExprWithTySig x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr HsGetField {} = emptyNodeComments
nodeCommentsHsExpr RecordUpd {..} = mconcat $ fmap nodeComments rupd_ext
nodeCommentsHsExpr RecordCon {..} = mconcat $ fmap nodeComments rcon_ext
nodeCommentsHsExpr (HsMultiIf x _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (ExplicitTuple x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr SectionR {} = emptyNodeComments
nodeCommentsHsExpr SectionL {} = emptyNodeComments
nodeCommentsHsExpr (NegApp x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr (OpApp x _ _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsExpr HsApp {} = emptyNodeComments
nodeCommentsHsExpr HsLit {} = emptyNodeComments
nodeCommentsHsExpr HsOverLit {} = emptyNodeComments
nodeCommentsHsExpr HsIPVar {} = emptyNodeComments
nodeCommentsHsExpr (HsUnboundVar x _) = fromMaybe mempty $ fmap nodeComments x
#else
nodeCommentsHsExpr (HsStatic x _) = nodeComments x
nodeCommentsHsExpr (HsProc x _ _) = nodeComments x
nodeCommentsHsExpr (ArithSeq x _ _) = nodeComments x
nodeCommentsHsExpr (ExprWithTySig x _ _) = nodeComments x
nodeCommentsHsExpr HsGetField {..} = nodeComments gf_ext
nodeCommentsHsExpr RecordUpd {..} = nodeComments rupd_ext
nodeCommentsHsExpr RecordCon {..} = nodeComments rcon_ext
nodeCommentsHsExpr (HsMultiIf x _) = nodeComments x
nodeCommentsHsExpr (ExplicitTuple x _ _) = nodeComments x
nodeCommentsHsExpr (SectionR x _ _) = nodeComments x
nodeCommentsHsExpr (SectionL x _ _) = nodeComments x
nodeCommentsHsExpr (NegApp x _ _) = nodeComments x
nodeCommentsHsExpr (OpApp x _ _ _) = nodeComments x
nodeCommentsHsExpr (HsApp x _ _) = nodeComments x
nodeCommentsHsExpr (HsLit x _) = nodeComments x
nodeCommentsHsExpr (HsOverLit x _) = nodeComments x
nodeCommentsHsExpr (HsIPVar x _) = nodeComments x
nodeCommentsHsExpr (HsUnboundVar x _) = nodeComments x
#endif
instance CommentExtraction (Match GhcPs a) where
  nodeComments = nodeCommentsMatch

nodeCommentsMatch :: Match GhcPs a -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsMatch Match {..} = mconcat $ fmap nodeComments m_ext
#else
nodeCommentsMatch Match {..} = nodeComments m_ext
#endif
instance CommentExtraction (StmtLR GhcPs GhcPs a) where
  nodeComments = nodeCommentsStmtLR

nodeCommentsStmtLR :: StmtLR GhcPs GhcPs a -> NodeComments
nodeCommentsStmtLR LastStmt {} = emptyNodeComments
nodeCommentsStmtLR ApplicativeStmt {} = emptyNodeComments
nodeCommentsStmtLR BodyStmt {} = emptyNodeComments
nodeCommentsStmtLR ParStmt {} = emptyNodeComments
nodeCommentsStmtLR RecStmt {..} = nodeComments recS_ext
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsStmtLR (BindStmt x _ _) = mconcat $ fmap nodeComments x
nodeCommentsStmtLR (LetStmt x _) = mconcat $ fmap nodeComments x
nodeCommentsStmtLR TransStmt {..} = mconcat $ fmap nodeComments trS_ext
#else
nodeCommentsStmtLR (BindStmt x _ _) = nodeComments x
nodeCommentsStmtLR (LetStmt x _) = nodeComments x
nodeCommentsStmtLR TransStmt {..} = nodeComments trS_ext
#endif
instance CommentExtraction (HsType GhcPs) where
  nodeComments = nodeCommentsHsType

nodeCommentsHsType :: HsType GhcPs -> NodeComments
nodeCommentsHsType HsForAllTy {} = emptyNodeComments
nodeCommentsHsType HsQualTy {} = emptyNodeComments
nodeCommentsHsType HsAppTy {} = emptyNodeComments
nodeCommentsHsType HsAppKindTy {} = emptyNodeComments
nodeCommentsHsType (HsListTy x _) = nodeComments x
nodeCommentsHsType (HsTupleTy x _ _) = nodeComments x
nodeCommentsHsType (HsSumTy x _) = nodeComments x
nodeCommentsHsType HsOpTy {} = emptyNodeComments
nodeCommentsHsType (HsParTy x _) = nodeComments x
nodeCommentsHsType HsStarTy {} = emptyNodeComments
nodeCommentsHsType HsSpliceTy {} = emptyNodeComments
nodeCommentsHsType (HsRecTy x _) = nodeComments x
nodeCommentsHsType HsTyLit {} = emptyNodeComments
nodeCommentsHsType HsWildCardTy {} = emptyNodeComments
nodeCommentsHsType XHsType {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsType (HsTyVar x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsType HsFunTy {} = emptyNodeComments
nodeCommentsHsType (HsIParamTy x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsType (HsKindSig x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsType (HsDocTy x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsType (HsBangTy x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsType (HsExplicitListTy x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsType (HsExplicitTupleTy x _) = mconcat $ fmap nodeComments x
#else
nodeCommentsHsType (HsTyVar x _ _) = nodeComments x
nodeCommentsHsType (HsFunTy x _ _ _) = nodeComments x
nodeCommentsHsType (HsIParamTy x _ _) = nodeComments x
nodeCommentsHsType (HsKindSig x _ _) = nodeComments x
nodeCommentsHsType (HsDocTy x _ _) = nodeComments x
nodeCommentsHsType (HsBangTy x _ _) = nodeComments x
nodeCommentsHsType (HsExplicitListTy x _ _) = nodeComments x
nodeCommentsHsType (HsExplicitTupleTy x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (HsMatchContext (GenLocated SrcSpanAnnN RdrName)) where
  nodeComments = nodeCommentsMatchContext
#else
instance CommentExtraction (HsMatchContext GhcPs) where
  nodeComments = nodeCommentsMatchContext
#endif

#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsMatchContext ::
     HsMatchContext (GenLocated SrcSpanAnnN RdrName) -> NodeComments
#else
nodeCommentsMatchContext :: HsMatchContext GhcPs -> NodeComments
#endif
nodeCommentsMatchContext FunRhs {} = emptyNodeComments
nodeCommentsMatchContext CaseAlt {} = emptyNodeComments
nodeCommentsMatchContext IfAlt {} = emptyNodeComments
nodeCommentsMatchContext ArrowMatchCtxt {} = emptyNodeComments
nodeCommentsMatchContext PatBindRhs {} = emptyNodeComments
nodeCommentsMatchContext PatBindGuards {} = emptyNodeComments
nodeCommentsMatchContext RecUpd {} = emptyNodeComments
nodeCommentsMatchContext StmtCtxt {} = emptyNodeComments
nodeCommentsMatchContext ThPatSplice {} = emptyNodeComments
nodeCommentsMatchContext ThPatQuote {} = emptyNodeComments
nodeCommentsMatchContext PatSyn {} = emptyNodeComments
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsMatchContext LambdaExpr {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1) && !MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsMatchContext LamCaseAlt {} = emptyNodeComments
#endif
instance CommentExtraction (Pat GhcPs) where
  nodeComments = nodeCommentsPat

nodeCommentsPat :: Pat GhcPs -> NodeComments
nodeCommentsPat WildPat {} = emptyNodeComments
nodeCommentsPat VarPat {} = emptyNodeComments
nodeCommentsPat (ListPat x _) = nodeComments x
nodeCommentsPat (SumPat x _ _ _) = nodeComments x
nodeCommentsPat SplicePat {} = emptyNodeComments
nodeCommentsPat LitPat {} = emptyNodeComments
nodeCommentsPat (NPlusKPat x _ _ _ _ _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat ParPat {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsPat (ParPat x _ _ _) = nodeComments x
#else
nodeCommentsPat (ParPat x _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat AsPat {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsPat (AsPat x _ _ _) = nodeComments x
#else
nodeCommentsPat (AsPat x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPat (LazyPat x _) = mconcat $ fmap nodeComments x
nodeCommentsPat (BangPat x _) = mconcat $ fmap nodeComments x
nodeCommentsPat (TuplePat x _ _) = mconcat $ fmap nodeComments x
nodeCommentsPat ConPat {..} = mconcat $ fmap nodeComments pat_con_ext
nodeCommentsPat (ViewPat x _ _) = mconcat $ fmap nodeComments x
nodeCommentsPat (NPat x _ _ _) = mconcat $ fmap nodeComments x
nodeCommentsPat (SigPat x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsPat (LazyPat x _) = nodeComments x
nodeCommentsPat (BangPat x _) = nodeComments x
nodeCommentsPat (TuplePat x _ _) = nodeComments x
nodeCommentsPat ConPat {..} = nodeComments pat_con_ext
nodeCommentsPat (ViewPat x _ _) = nodeComments x
nodeCommentsPat (NPat x _ _ _) = nodeComments x
nodeCommentsPat (SigPat x _ _) = nodeComments x
#endif
instance CommentExtraction (HsTupArg GhcPs) where
  nodeComments = nodeCommentsHsTupArg

nodeCommentsHsTupArg :: HsTupArg GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsTupArg Present {} = emptyNodeComments
#else
nodeCommentsHsTupArg (Present x _) = nodeComments x
#endif
nodeCommentsHsTupArg (Missing x) = nodeComments x

instance CommentExtraction (ConDeclField GhcPs) where
  nodeComments = nodeCommentsConDeclField

nodeCommentsConDeclField :: ConDeclField GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsConDeclField ConDeclField {..} =
  mconcat $ fmap nodeComments cd_fld_ext
#else
nodeCommentsConDeclField ConDeclField {..} = nodeComments cd_fld_ext
#endif
instance CommentExtraction (HsDerivingClause GhcPs) where
  nodeComments = nodeCommentsHsDerivingClause

nodeCommentsHsDerivingClause :: HsDerivingClause GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsDerivingClause HsDerivingClause {..} =
  mconcat $ fmap nodeComments deriv_clause_ext
#else
nodeCommentsHsDerivingClause HsDerivingClause {..} =
  nodeComments deriv_clause_ext
#endif
-- | This instance is for type family declarations inside a class declaration.
instance CommentExtraction (FamilyDecl GhcPs) where
  nodeComments = nodeCommentsFamilyDecl

nodeCommentsFamilyDecl :: FamilyDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsFamilyDecl FamilyDecl {..} = mconcat $ fmap nodeComments fdExt
#else
nodeCommentsFamilyDecl FamilyDecl {..} = nodeComments fdExt
#endif
instance CommentExtraction (HsTyVarBndr a GhcPs) where
  nodeComments = nodeCommentsHsTyVarBndr

nodeCommentsHsTyVarBndr :: HsTyVarBndr a GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsTyVarBndr (UserTyVar x _ _) = mconcat $ fmap nodeComments x
nodeCommentsHsTyVarBndr (KindedTyVar x _ _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsHsTyVarBndr (UserTyVar x _ _) = nodeComments x
nodeCommentsHsTyVarBndr (KindedTyVar x _ _ _) = nodeComments x
#endif
instance CommentExtraction (IE GhcPs) where
  nodeComments = nodeCommentsIE

nodeCommentsIE :: IE GhcPs -> NodeComments
nodeCommentsIE IEVar {} = emptyNodeComments
nodeCommentsIE IEGroup {} = emptyNodeComments
nodeCommentsIE IEDoc {} = emptyNodeComments
nodeCommentsIE IEDocNamed {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsIE (IEThingAbs _ x _) = nodeComments x
nodeCommentsIE (IEThingAll _ x _) = nodeComments x
nodeCommentsIE (IEThingWith _ x _ _ _) = nodeComments x
nodeCommentsIE (IEModuleContents (x, y) _) =
  mconcat $ maybeToList (fmap nodeComments x) <> fmap nodeComments y
#elif MIN_VERSION_ghc_lib_parser(9, 8, 1)
nodeCommentsIE (IEThingAbs (_, x) _) = nodeComments x
nodeCommentsIE (IEThingAll (_, x) _) = nodeComments x
nodeCommentsIE (IEThingWith (_, x) _ _ _) = nodeComments x
nodeCommentsIE (IEModuleContents (_, x) _) = nodeComments x
#else
nodeCommentsIE (IEThingAbs x _) = nodeComments x
nodeCommentsIE (IEThingAll x _) = nodeComments x
nodeCommentsIE (IEThingWith x _ _ _) = nodeComments x
nodeCommentsIE (IEModuleContents x _) = nodeComments x
#endif
instance CommentExtraction (FamEqn GhcPs a) where
  nodeComments = nodeCommentsFamEqn

nodeCommentsFamEqn :: FamEqn GhcPs a -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsFamEqn FamEqn {..} = mconcat $ fmap nodeComments feqn_ext
#else
nodeCommentsFamEqn FamEqn {..} = nodeComments feqn_ext
#endif
instance CommentExtraction (WarnDecls GhcPs) where
  nodeComments = nodeCommentsWarnDecls

nodeCommentsWarnDecls :: WarnDecls GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsWarnDecls Warnings {..} = mconcat $ fmap nodeComments $ fst wd_ext
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsWarnDecls Warnings {..} = nodeComments $ fst wd_ext
#else
nodeCommentsWarnDecls Warnings {..} = nodeComments wd_ext
#endif
instance CommentExtraction (WarnDecl GhcPs) where
  nodeComments = nodeCommentsWarnDecl

nodeCommentsWarnDecl :: WarnDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsWarnDecl (Warning (_, x) _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsWarnDecl (Warning x _ _) = nodeComments x
#endif
instance CommentExtraction (RuleDecls GhcPs) where
  nodeComments = nodeCommentsRuleDecls

nodeCommentsRuleDecls :: RuleDecls GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsRuleDecls HsRules {..} = mconcat $ fmap nodeComments $ fst rds_ext
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsRuleDecls HsRules {..} = nodeComments $ fst rds_ext
#else
nodeCommentsRuleDecls HsRules {..} = nodeComments rds_ext
#endif
instance CommentExtraction (RuleDecl GhcPs) where
  nodeComments = nodeCommentsRuleDecl

nodeCommentsRuleDecl :: RuleDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsRuleDecl HsRule {..} = nodeComments $ fst rd_ext
#else
nodeCommentsRuleDecl HsRule {..} = nodeComments rd_ext
#endif
instance CommentExtraction (DerivDecl GhcPs) where
  nodeComments = nodeCommentsDerivDecl

nodeCommentsDerivDecl :: DerivDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsDerivDecl DerivDecl {deriv_ext = (x, xs)} =
  mconcat $ maybeToList (fmap nodeComments x) <> fmap nodeComments xs
#else
nodeCommentsDerivDecl DerivDecl {..} = nodeComments deriv_ext
#endif
instance CommentExtraction (StandaloneKindSig GhcPs) where
  nodeComments = nodeCommentsStandaloneKindSig

nodeCommentsStandaloneKindSig :: StandaloneKindSig GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsStandaloneKindSig (StandaloneKindSig x _ _) =
  mconcat $ fmap nodeComments x
#else
nodeCommentsStandaloneKindSig (StandaloneKindSig x _ _) = nodeComments x
#endif
instance CommentExtraction (DefaultDecl GhcPs) where
  nodeComments = nodeCommentsDefaultDecl

nodeCommentsDefaultDecl :: DefaultDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsDefaultDecl (DefaultDecl x _) = mconcat $ fmap nodeComments x
#else
nodeCommentsDefaultDecl (DefaultDecl x _) = nodeComments x
#endif
instance CommentExtraction (ForeignDecl GhcPs) where
  nodeComments = nodeCommentsForeignDecl

nodeCommentsForeignDecl :: ForeignDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsForeignDecl ForeignImport {..} =
  mconcat $ fmap nodeComments fd_i_ext
nodeCommentsForeignDecl ForeignExport {..} =
  mconcat $ fmap nodeComments fd_e_ext
#else
nodeCommentsForeignDecl ForeignImport {..} = nodeComments fd_i_ext
nodeCommentsForeignDecl ForeignExport {..} = nodeComments fd_e_ext
#endif
instance CommentExtraction (AnnDecl GhcPs) where
  nodeComments = nodeCommentsAnnDecl

nodeCommentsAnnDecl :: AnnDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsAnnDecl (HsAnnotation (x, _) _ _) = nodeComments x
#else
nodeCommentsAnnDecl (HsAnnotation x _ _ _) = nodeComments x
#endif
instance CommentExtraction (RoleAnnotDecl GhcPs) where
  nodeComments = nodeCommentsRoleAnnotDecl

nodeCommentsRoleAnnotDecl :: RoleAnnotDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsRoleAnnotDecl (RoleAnnotDecl x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsRoleAnnotDecl (RoleAnnotDecl x _ _) = nodeComments x
#endif
instance CommentExtraction (TyFamInstDecl GhcPs) where
  nodeComments = nodeCommentsTyFamInstDecl

nodeCommentsTyFamInstDecl :: TyFamInstDecl GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsTyFamInstDecl TyFamInstDecl {..} =
  mconcat $ fmap nodeComments tfid_xtn
#else
nodeCommentsTyFamInstDecl TyFamInstDecl {..} = nodeComments tfid_xtn
#endif
instance CommentExtraction (PatSynBind GhcPs GhcPs) where
  nodeComments = nodeCommentsPatSynBind

nodeCommentsPatSynBind :: PatSynBind GhcPs GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsPatSynBind PSB {..} = mconcat $ fmap nodeComments psb_ext
#else
nodeCommentsPatSynBind PSB {..} = nodeComments psb_ext
#endif
instance CommentExtraction InlineSpec where
  nodeComments = nodeCommentsInlineSpec

nodeCommentsInlineSpec :: InlineSpec -> NodeComments
nodeCommentsInlineSpec Inline {} = emptyNodeComments
nodeCommentsInlineSpec Inlinable {} = emptyNodeComments
nodeCommentsInlineSpec NoInline {} = emptyNodeComments
nodeCommentsInlineSpec NoUserInlinePrag {} = emptyNodeComments
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsInlineSpec Opaque {} = emptyNodeComments
#endif
instance CommentExtraction (HsPragE GhcPs) where
  nodeComments = nodeCommentsHsPragE

nodeCommentsHsPragE :: HsPragE GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
nodeCommentsHsPragE (HsPragSCC (x, _) _) = nodeComments x
#else
nodeCommentsHsPragE (HsPragSCC x _ _) = nodeComments x
#endif
instance CommentExtraction (IPBind GhcPs) where
  nodeComments = nodeCommentsIPBind

nodeCommentsIPBind :: IPBind GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsIPBind (IPBind x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsIPBind (IPBind x _ _) = nodeComments x
#endif
instance CommentExtraction (DerivStrategy GhcPs) where
  nodeComments = nodeCommentsDerivStrategy

nodeCommentsDerivStrategy :: DerivStrategy GhcPs -> NodeComments
nodeCommentsDerivStrategy (ViaStrategy x) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsDerivStrategy (StockStrategy x) = mconcat $ fmap nodeComments x
nodeCommentsDerivStrategy (AnyclassStrategy x) = mconcat $ fmap nodeComments x
nodeCommentsDerivStrategy (NewtypeStrategy x) = mconcat $ fmap nodeComments x
#else
nodeCommentsDerivStrategy (StockStrategy x) = nodeComments x
nodeCommentsDerivStrategy (AnyclassStrategy x) = nodeComments x
nodeCommentsDerivStrategy (NewtypeStrategy x) = nodeComments x
#endif
instance CommentExtraction XViaStrategyPs where
  nodeComments = nodeCommentsXViaStrategyPs

nodeCommentsXViaStrategyPs :: XViaStrategyPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsXViaStrategyPs (XViaStrategyPs x _) = mconcat $ fmap nodeComments x
#else
nodeCommentsXViaStrategyPs (XViaStrategyPs x _) = nodeComments x
#endif
instance CommentExtraction (HsCmd GhcPs) where
  nodeComments = nodeCommentsHsCmd

nodeCommentsHsCmd :: HsCmd GhcPs -> NodeComments
nodeCommentsHsCmd (HsCmdArrApp x _ _ _ _) = nodeComments x
nodeCommentsHsCmd (HsCmdArrForm x _ _ _ _) = nodeComments x
nodeCommentsHsCmd HsCmdLam {} = emptyNodeComments
nodeCommentsHsCmd (HsCmdCase x _ _) = nodeComments x
nodeCommentsHsCmd (HsCmdIf x _ _ _ _) = nodeComments x
nodeCommentsHsCmd (HsCmdDo x _) = nodeComments x
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsCmd HsCmdApp {} = emptyNodeComments
#else
nodeCommentsHsCmd (HsCmdApp x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsCmd HsCmdPar {} = emptyNodeComments
nodeCommentsHsCmd HsCmdLet {} = emptyNodeComments
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
nodeCommentsHsCmd (HsCmdPar x _ _ _) = nodeComments x
nodeCommentsHsCmd (HsCmdLamCase x _ _) = nodeComments x
nodeCommentsHsCmd (HsCmdLet x _ _ _ _) = nodeComments x
#else
nodeCommentsHsCmd (HsCmdPar x _) = nodeComments x
nodeCommentsHsCmd (HsCmdLamCase x _) = nodeComments x
nodeCommentsHsCmd (HsCmdLet x _ _) = nodeComments x
#endif
instance CommentExtraction (RuleBndr GhcPs) where
  nodeComments = nodeCommentsRuleBndr

nodeCommentsRuleBndr :: RuleBndr GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsRuleBndr (RuleBndr x _) = mconcat $ fmap nodeComments x
nodeCommentsRuleBndr (RuleBndrSig x _ _) = mconcat $ fmap nodeComments x
#else
nodeCommentsRuleBndr (RuleBndr x _) = nodeComments x
nodeCommentsRuleBndr (RuleBndrSig x _ _) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction FieldLabelString where
  nodeComments = const emptyNodeComments

instance CommentExtraction (HsUntypedSplice GhcPs) where
  nodeComments = nodeCommentsHsUntypedSplice

nodeCommentsHsUntypedSplice :: HsUntypedSplice GhcPs -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsUntypedSplice (HsUntypedSpliceExpr x _) =
  mconcat $ fmap nodeComments x
#else
nodeCommentsHsUntypedSplice (HsUntypedSpliceExpr x _) = nodeComments x
#endif
nodeCommentsHsUntypedSplice HsQuasiQuote {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
instance CommentExtraction (HsQuote GhcPs) where
  nodeComments ExpBr {} = emptyNodeComments
  nodeComments PatBr {} = emptyNodeComments
  nodeComments DecBrL {} = emptyNodeComments
  nodeComments DecBrG {} = emptyNodeComments
  nodeComments TypBr {} = emptyNodeComments
  nodeComments VarBr {} = emptyNodeComments

instance CommentExtraction (WithHsDocIdentifiers StringLiteral GhcPs) where
  nodeComments WithHsDocIdentifiers {} = emptyNodeComments

instance CommentExtraction (DotFieldOcc GhcPs) where
  nodeComments DotFieldOcc {..} = nodeComments dfoExt

instance CommentExtraction (HsFieldBind a b) where
  nodeComments = nodeCommentsHsFieldBind

nodeCommentsHsFieldBind :: HsFieldBind a b -> NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
nodeCommentsHsFieldBind HsFieldBind {..} = mconcat $ fmap nodeComments hfbAnn
#else
nodeCommentsHsFieldBind HsFieldBind {..} = nodeComments hfbAnn
#endif
#else
instance CommentExtraction (HsFieldLabel GhcPs) where
  nodeComments HsFieldLabel {..} = nodeComments hflExt

instance CommentExtraction RecConField where
  nodeComments (RecConField x) = nodeComments x
#endif
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
instance CommentExtraction RecConField where
  nodeComments (RecConField x) = nodeComments x
#else
instance CommentExtraction (HsBracket GhcPs) where
  nodeComments ExpBr {} = emptyNodeComments
  nodeComments PatBr {} = emptyNodeComments
  nodeComments DecBrL {} = emptyNodeComments
  nodeComments DecBrG {} = emptyNodeComments
  nodeComments TypBr {} = emptyNodeComments
  nodeComments VarBr {} = emptyNodeComments
  nodeComments TExpBr {} = emptyNodeComments

instance CommentExtraction (HsRecField' (FieldOcc GhcPs) a) where
  nodeComments HsRecField {..} = nodeComments hsRecFieldAnn
#endif
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance CommentExtraction XImportDeclPass where
  nodeComments XImportDeclPass {..} = nodeComments ideclAnn

instance CommentExtraction (ForeignImport GhcPs) where
  nodeComments CImport {} = emptyNodeComments

instance CommentExtraction (ForeignExport GhcPs) where
  nodeComments CExport {} = emptyNodeComments

instance CommentExtraction (HsTyLit GhcPs) where
  nodeComments HsNumTy {} = emptyNodeComments
  nodeComments HsStrTy {} = emptyNodeComments
  nodeComments HsCharTy {} = emptyNodeComments
#else
instance CommentExtraction (HsSplice GhcPs) where
  nodeComments (HsTypedSplice x _ _ _) = nodeComments x
  nodeComments (HsUntypedSplice x _ _ _) = nodeComments x
  nodeComments HsQuasiQuote {} = emptyNodeComments
  nodeComments HsSpliced {} = emptyNodeComments

instance CommentExtraction ForeignImport where
  nodeComments CImport {} = emptyNodeComments

instance CommentExtraction ForeignExport where
  nodeComments CExport {} = emptyNodeComments

instance CommentExtraction HsTyLit where
  nodeComments HsNumTy {} = emptyNodeComments
  nodeComments HsStrTy {} = emptyNodeComments
  nodeComments HsCharTy {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
instance CommentExtraction
           (HsArg
              GhcPs
              (GenLocated SrcSpanAnnA (HsType GhcPs))
              (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  nodeComments HsValArg {} = emptyNodeComments
  nodeComments HsTypeArg {} = emptyNodeComments
  nodeComments HsArgPar {} = emptyNodeComments

instance CommentExtraction (LHsRecUpdFields GhcPs) where
  nodeComments RegularRecUpdFields {} = emptyNodeComments
  nodeComments OverloadedRecUpdFields {} = emptyNodeComments
#else
instance CommentExtraction
           (HsArg
              (GenLocated SrcSpanAnnA (HsType GhcPs))
              (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  nodeComments HsValArg {} = emptyNodeComments
  nodeComments HsTypeArg {} = emptyNodeComments
  nodeComments HsArgPar {} = emptyNodeComments
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance CommentExtraction (EpAnn a) where
  nodeComments (EpAnn ann _ cs) = NodeComments {..}
    where
      commentsBefore = priorComments cs
      commentsOnSameLine = filter isCommentOnSameLine $ getFollowingComments cs
      commentsAfter =
        filter (not . isCommentOnSameLine) $ getFollowingComments cs
      isCommentOnSameLine (L comAnn _) =
        srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)

instance CommentExtraction (EpaLocation' NoComments) where
  nodeComments EpaSpan {} = emptyNodeComments
  nodeComments EpaDelta {} = emptyNodeComments

instance CommentExtraction AnnList where
  nodeComments AnnList {..} = mconcat [a, b, c, d, e]
    where
      a = maybe mempty nodeComments al_anchor
      b = maybe mempty nodeComments al_open
      c = maybe mempty nodeComments al_close
      d = mconcat $ fmap nodeComments al_rest
      e = mconcat $ fmap nodeComments al_trailing

instance CommentExtraction TrailingAnn where
  nodeComments AddSemiAnn {..} = nodeComments ta_location
  nodeComments AddCommaAnn {..} = nodeComments ta_location
  nodeComments AddVbarAnn {..} = nodeComments ta_location
  nodeComments AddDarrowAnn {..} = nodeComments ta_location
  nodeComments AddDarrowUAnn {..} = nodeComments ta_location

instance CommentExtraction AnnParen where
  nodeComments AnnParen {..} = mconcat $ fmap nodeComments [ap_open, ap_close]
#else
instance CommentExtraction Anchor where
  nodeComments Anchor {} = emptyNodeComments

instance CommentExtraction (SrcAnn a) where
  nodeComments (SrcSpanAnn ep _) = nodeComments ep

instance CommentExtraction (EpAnn a) where
  nodeComments (EpAnn ann _ cs) = NodeComments {..}
    where
      commentsBefore = priorComments cs
      commentsOnSameLine = filter isCommentOnSameLine $ getFollowingComments cs
      commentsAfter =
        filter (not . isCommentOnSameLine) $ getFollowingComments cs
      isCommentOnSameLine (L comAnn _) =
        srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
  nodeComments EpAnnNotUsed = emptyNodeComments
#endif
-- | Marks an AST node as never appearing in the AST.
--
-- Some AST node types are only used in the renaming or type-checking phase.
notUsedInParsedStage :: HasCallStack => a
notUsedInParsedStage =
  error
    "This AST should never appears in an AST. It only appears in the renaming or type checked stages."

-- | A 'NodeComment' with no comments.
emptyNodeComments :: NodeComments
emptyNodeComments = NodeComments [] [] []
