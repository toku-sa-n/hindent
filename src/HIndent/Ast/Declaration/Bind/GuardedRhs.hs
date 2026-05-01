{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Bind.GuardedRhs
  ( GuardedRhs
  , addWhereComments
  , mkGuardedRhs
  , mkCaseGuardedRhs
  , mkLambdaGuardedRhs
  , mkMultiWayIfGuardedRhs
  , mkCaseCmdGuardedRhs
  , mkLambdaCmdGuardedRhs
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
import HIndent.Ast.LocalBinds (LocalBinds, mkLocalBinds)
import qualified HIndent.Ast.NodeComments as NodeComments
import HIndent.Ast.WithComments
  ( WithComments
  , addComments
  , flattenComments
  , fromGenLocated
  , mkWithComments
  , prettyWith
  )
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data GuardedRhs = GuardedRhs
  { guards :: [WithComments Guard]
  , whereComments :: NodeComments.NodeComments
  , localBinds :: Maybe (WithComments LocalBinds)
  }

instance Pretty GuardedRhs where
  pretty' GuardedRhs {..} = do
    mapM_ pretty guards
    whenJust localBinds $ \lbs ->
      indentedBlock
        $ newlinePrefixed
        $ whereComment
            <> [string "where", prettyWith lbs $ indentedBlock . pretty]
    where
      whereComment
        | whereComments == mempty = []
        | otherwise =
          [ prettyWith
              (addComments whereComments $ mkWithComments ())
              (const $ pure ())
          ]

addWhereComments :: NodeComments.NodeComments -> GuardedRhs -> GuardedRhs
addWhereComments extra guardedRhs =
  guardedRhs {whereComments = extra <> whereComments guardedRhs}

mkGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkExprGuard . fromGenLocated)
          (toList grhssGRHSs)
    , whereComments = NodeComments.fromEpAnnComments grhssExt
    , localBinds = mkLocalBinds grhssLocalBinds
    }

mkCaseGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkCaseGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkCaseExprGuard . fromGenLocated)
          (toList grhssGRHSs)
    , whereComments = NodeComments.fromEpAnnComments grhssExt
    , localBinds = mkLocalBinds grhssLocalBinds
    }

mkLambdaGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkLambdaGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkLambdaExprGuard . fromGenLocated)
          (toList grhssGRHSs)
    , whereComments = NodeComments.fromEpAnnComments grhssExt
    , localBinds = mkLocalBinds grhssLocalBinds
    }

mkMultiWayIfGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GuardedRhs
mkMultiWayIfGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkMultiWayIfExprGuard . fromGenLocated)
          (toList grhssGRHSs)
    , whereComments = NodeComments.fromEpAnnComments grhssExt
    , localBinds = mkLocalBinds grhssLocalBinds
    }

mkCaseCmdGuardedRhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkCaseCmdGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkCaseCmdGuard . fromGenLocated)
          (toList grhssGRHSs)
    , whereComments = NodeComments.fromEpAnnComments grhssExt
    , localBinds = mkLocalBinds grhssLocalBinds
    }

mkLambdaCmdGuardedRhs ::
     GHC.GRHSs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> GuardedRhs
mkLambdaCmdGuardedRhs GHC.GRHSs {..} =
  GuardedRhs
    { guards =
        fmap
          (flattenComments . fmap mkLambdaCmdGuard . fromGenLocated)
          (toList grhssGRHSs)
    , whereComments = NodeComments.fromEpAnnComments grhssExt
    , localBinds = mkLocalBinds grhssLocalBinds
    }
