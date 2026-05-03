{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module HIndent.Ast.Guard
  ( Guard
  , mkExprGuard
  , mkCaseExprGuard
  , mkLambdaExprGuard
  , mkMultiWayIfExprGuard
  , mkCaseCmdGuard
  , mkLambdaCmdGuard
  ) where

import Control.Monad (unless)
import HIndent.Ast.Cmd (Cmd, mkCmd)
import {-# SOURCE #-} HIndent.Ast.Expression
  ( GuardExpression
  , mkExpression
  , mkGuardExpression
  )
import qualified HIndent.Ast.NodeComments as NodeComments
import HIndent.Ast.Statement (ExprStatement, mkExprStatement)
import HIndent.Ast.WithComments
  ( WithComments
  , addComments
  , fromGenLocated
  , mkWithComments
  )
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data GuardContext
  = PlainGuard
  | CaseGuard
  | LambdaGuard
  | MultiWayIfGuard
  deriving (Eq)

data Guard
  = ExprGuard
      { guardContext :: GuardContext
      , conditions :: [WithComments ExprStatement]
      , expr :: WithComments GuardExpression
      }
  | CmdGuard
      { guardContext :: GuardContext
      , conditions :: [WithComments ExprStatement]
      , cmd :: WithComments Cmd
      }

instance Pretty Guard where
  pretty ExprGuard {..}
    | null conditions = do
      space
      string (contextSeparator guardContext)
      pretty expr
    | otherwise = do
      unless (guardContext == MultiWayIfGuard) newline
      (if guardContext == MultiWayIfGuard
         then id
         else indentedBlock) $ do
        string "| " |=> vCommaSep (fmap pretty conditions)
        space
        string (contextSeparator guardContext)
        pretty expr
  pretty CmdGuard {..}
    | null conditions = do
      space
      string (contextSeparator guardContext)
      let hor = space >> pretty cmd
          ver = newline >> indentedBlock (pretty cmd)
       in hor <-|> ver
    | otherwise = do
      unless (guardContext == MultiWayIfGuard) newline
      (if guardContext == MultiWayIfGuard
         then id
         else indentedBlock) $ do
        string "| " |=> vCommaSep (fmap pretty conditions)
        space
        string (contextSeparator guardContext)
        let hor = space >> pretty cmd
            ver = newline >> indentedBlock (pretty cmd)
         in hor <-|> ver

contextSeparator :: GuardContext -> String
contextSeparator PlainGuard = "="
contextSeparator CaseGuard = "->"
contextSeparator LambdaGuard = "->"
contextSeparator MultiWayIfGuard = "->"

mkExprGuard :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> WithComments Guard
mkExprGuard (GHC.GRHS ann conditions resultExpr) =
  addComments (NodeComments.fromEpAnn ann)
    $ mkWithComments
        ExprGuard
          { guardContext = PlainGuard
          , conditions = fmap (fmap mkExprStatement . fromGenLocated) conditions
          , expr =
              mkGuardExpression . mkExpression <$> fromGenLocated resultExpr
          }

mkCaseExprGuard ::
     GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> WithComments Guard
mkCaseExprGuard (GHC.GRHS ann conditions resultExpr) =
  addComments (NodeComments.fromEpAnn ann)
    $ mkWithComments
        ExprGuard
          { guardContext = CaseGuard
          , conditions = fmap (fmap mkExprStatement . fromGenLocated) conditions
          , expr =
              mkGuardExpression . mkExpression <$> fromGenLocated resultExpr
          }

mkLambdaExprGuard ::
     GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> WithComments Guard
mkLambdaExprGuard (GHC.GRHS ann conditions resultExpr) =
  addComments (NodeComments.fromEpAnn ann)
    $ mkWithComments
        ExprGuard
          { guardContext = LambdaGuard
          , conditions = fmap (fmap mkExprStatement . fromGenLocated) conditions
          , expr =
              mkGuardExpression . mkExpression <$> fromGenLocated resultExpr
          }

mkMultiWayIfExprGuard ::
     GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> WithComments Guard
mkMultiWayIfExprGuard (GHC.GRHS ann conditions resultExpr) =
  addComments (NodeComments.fromEpAnn ann)
    $ mkWithComments
        ExprGuard
          { guardContext = MultiWayIfGuard
          , conditions = fmap (fmap mkExprStatement . fromGenLocated) conditions
          , expr =
              mkGuardExpression . mkExpression <$> fromGenLocated resultExpr
          }

mkCaseCmdGuard ::
     GHC.GRHS GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> WithComments Guard
mkCaseCmdGuard (GHC.GRHS ann conditions cmd) =
  addComments (NodeComments.fromEpAnn ann)
    $ mkWithComments
        CmdGuard
          { guardContext = CaseGuard
          , conditions = fmap (fmap mkExprStatement . fromGenLocated) conditions
          , cmd = fmap mkCmd (fromGenLocated cmd)
          }

mkLambdaCmdGuard ::
     GHC.GRHS GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> WithComments Guard
mkLambdaCmdGuard (GHC.GRHS ann conditions cmd) =
  addComments (NodeComments.fromEpAnn ann)
    $ mkWithComments
        CmdGuard
          { guardContext = LambdaGuard
          , conditions = fmap (fmap mkExprStatement . fromGenLocated) conditions
          , cmd = fmap mkCmd (fromGenLocated cmd)
          }
