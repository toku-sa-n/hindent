{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Bind.GuardedRhs
  ( GuardedRhs
  , mkExprGuardedRhs
  , mkCmdGuardedRhs
  , mkPatternGuardedRhs
  , mkMultiWayIfGuardedRhs
  ) where

import Data.Foldable (toList)
import HIndent.Applicative (whenJust)
import HIndent.Ast.Guard
  ( Guard
  , mkCaseCmdGuard
  , mkCaseExprGuard
  , mkExprGuard
  , mkLambdaCmdGuard
  , mkLambdaExprGuard
  , mkMultiWayIfExprGuard
  )
import qualified HIndent.Ast.WhereClause as WhereClause
import HIndent.Ast.WithComments (WithComments, flattenComments, fromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty

data GuardedRhs = GuardedRhs
  { guards :: [WithComments Guard]
  , whereClause :: Maybe (WithComments WhereClause.WhereClause)
  }

instance Pretty GuardedRhs where
  pretty' GuardedRhs {..} = do
    mapM_ pretty guards
    whenJust whereClause WhereClause.prettyWhereClause

mkExprGuardedRhs :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamSingle, ..} =
  mkLambdaExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamAlt _, ..} =
  mkCaseExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  mkCaseExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.FunRhs {}, ..} =
  mkFunctionExprGuardedRhs' match
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamSingle, ..} =
  mkLambdaExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamCase, ..} =
  mkCaseExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamCases, ..} =
  mkCaseExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  mkCaseExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.FunRhs {}, ..} =
  mkFunctionExprGuardedRhs' match
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LambdaExpr, ..} =
  mkLambdaExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamCaseAlt {}, ..} =
  mkCaseExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  mkCaseExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.FunRhs {}, ..} =
  mkFunctionExprGuardedRhs' match
#else
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LambdaExpr, ..} =
  mkLambdaExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  mkCaseExprGuardedRhs' match
mkExprGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.FunRhs {}, ..} =
  mkFunctionExprGuardedRhs' match
#endif
mkExprGuardedRhs _ = error "`ghc-lib-parser` never generates this AST node."

mkCmdGuardedRhs :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamSingle, ..} =
  mkLambdaCmdGuardedRhs' match
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamAlt _, ..} =
  mkCaseCmdGuardedRhs' match
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  mkCaseCmdGuardedRhs' match
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamSingle, ..} =
  mkLambdaCmdGuardedRhs' match
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamCase, ..} =
  mkCaseCmdGuardedRhs' match
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamCases, ..} =
  mkCaseCmdGuardedRhs' match
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  mkCaseCmdGuardedRhs' match
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LambdaExpr, ..} =
  mkLambdaCmdGuardedRhs' match
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LamCaseAlt {}, ..} =
  mkCaseCmdGuardedRhs' match
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  mkCaseCmdGuardedRhs' match
#else
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.LambdaExpr, ..} =
  mkLambdaCmdGuardedRhs' match
mkCmdGuardedRhs match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  mkCaseCmdGuardedRhs' match
#endif
mkCmdGuardedRhs _ = error "`ghc-lib-parser` never generates this AST node."

mkPatternGuardedRhs :: GHC.HsBind GHC.GhcPs -> GuardedRhs
mkPatternGuardedRhs bind@GHC.PatBind {GHC.pat_rhs = grhss} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkExprGuard . fromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = WhereClause.mkPatternWhereClause bind
    }
mkPatternGuardedRhs _ = error "This AST node should not appear."

mkMultiWayIfGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkMultiWayIfGuardedRhs grhss@GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkMultiWayIfExprGuard . fromGenLocated)
          (toList grhssGRHSs)
    , whereClause = WhereClause.mkWhereClause grhss
    }

mkLambdaExprGuardedRhs' ::
     GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkLambdaExprGuardedRhs' match@GHC.Match {GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkLambdaExprGuard . fromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = WhereClause.mkMatchWhereClause match
    }

mkCaseExprGuardedRhs' ::
     GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkCaseExprGuardedRhs' match@GHC.Match {GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkCaseExprGuard . fromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = WhereClause.mkMatchWhereClause match
    }

mkFunctionExprGuardedRhs' ::
     GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkFunctionExprGuardedRhs' match@GHC.Match {GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkExprGuard . fromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = WhereClause.mkMatchWhereClause match
    }

mkLambdaCmdGuardedRhs' ::
     GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkLambdaCmdGuardedRhs' match@GHC.Match {GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkLambdaCmdGuard . fromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = WhereClause.mkMatchWhereClause match
    }

mkCaseCmdGuardedRhs' :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkCaseCmdGuardedRhs' match@GHC.Match {GHC.m_grhss = grhss} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkCaseCmdGuard . fromGenLocated)
          (toList $ GHC.grhssGRHSs grhss)
    , whereClause = WhereClause.mkMatchWhereClause match
    }
