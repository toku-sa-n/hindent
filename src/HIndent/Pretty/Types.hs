{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Types to pretty-print certain parts of Haskell codes.
--
-- We define new types to pretty-print AST nodes rather than define
-- functions to print comments easily using the 'Pretty' implementation of
-- 'GenLocated'.
module HIndent.Pretty.Types
  ( InfixExpr(..)
  , InfixOp(..)
  , PrefixOp(..)
  , InfixApp(..)
  , GRHSsExpr(..)
  , GRHSsProc(..)
  , GRHSExpr(..)
  , GRHSProc(..)
  , RecConPat(..)
  , RecConField(..)
  , HsSigTypeInsideInstDecl(..)
  , HsSigTypeInsideVerticalFuncSig(..)
  , HsSigTypeInsideDeclSig(..)
  , HsType'(..)
  , pattern HsTypeInsideVerticalFuncSig
  , pattern HsTypeInsideDeclSig
  , StmtLRInsideVerticalList(..)
  , ParStmtBlockInsideVerticalList(..)
  , DeclSig(..)
  , TopLevelTyFamInstDecl(..)
  , Context(..)
  , HorizontalContext(..)
  , VerticalContext(..)
  , ModuleNameWithPrefix(..)
  , PatInsidePatDecl(..)
  , LambdaCase(..)
  , ModuleDeprecatedPragma(..)
  , ListComprehension(..)
  , DoExpression(..)
  , DoOrMdo(..)
  , LetIn(..)
  , NodeComments(..)
  , GRHSExprType(..)
  , GRHSProcType(..)
  , HsTypeFor(..)
  , HsTypeDir(..)
  ) where

import           GHC.Hs
import           GHC.Types.Name.Reader
import           GHC.Unit
import           GHC.Unit.Module.Warnings

newtype InfixExpr =
  InfixExpr (LHsExpr GhcPs)

newtype InfixOp =
  InfixOp RdrName

-- | A wrapper type for printing an identifier as a prefix operator.
--
-- Printing a `PrefixOp` value containing a symbol operator wraps it with
-- parentheses.
newtype PrefixOp =
  PrefixOp RdrName

data InfixApp =
  InfixApp
    { lhs                :: LHsExpr GhcPs
    , op                 :: LHsExpr GhcPs
    , rhs                :: LHsExpr GhcPs
    , immediatelyAfterDo :: Bool
    }

data GRHSsExpr =
  GRHSsExpr
    { grhssExprType :: GRHSExprType
    , grhssExpr     :: GRHSs GhcPs (LHsExpr GhcPs)
    }

newtype GRHSsProc =
  GRHSsProc (GRHSs GhcPs (LHsCmd GhcPs))

-- | 'GRHS' for a normal binding.
data GRHSExpr =
  GRHSExpr
    { grhsExprType :: GRHSExprType
    , grhsExpr     :: GRHS GhcPs (LHsExpr GhcPs)
    }

-- | 'GRHS' for a @proc@ binding.
newtype GRHSProc =
  GRHSProc (GRHS GhcPs (LHsCmd GhcPs))

newtype RecConPat =
  RecConPat (HsRecFields GhcPs (LPat GhcPs))
#if MIN_VERSION_ghc_lib_parser(9,4,1)
newtype RecConField =
  RecConField (HsFieldBind (LFieldOcc GhcPs) (LPat GhcPs))
#else
newtype RecConField =
  RecConField (HsRecField' (FieldOcc GhcPs) (LPat GhcPs))
#endif
newtype HsSigTypeInsideInstDecl =
  HsSigTypeInsideInstDecl (HsSigType GhcPs)

newtype HsSigTypeInsideVerticalFuncSig =
  HsSigTypeInsideVerticalFuncSig (HsSigType GhcPs)

newtype HsSigTypeInsideDeclSig =
  HsSigTypeInsideDeclSig (HsSigType GhcPs)

data HsType' =
  HsType'
    { hsTypeFor :: HsTypeFor
    , hsTypeDir :: HsTypeDir
    , hsType    :: HsType GhcPs
    }

pattern HsTypeInsideVerticalFuncSig :: HsType GhcPs -> HsType'
pattern HsTypeInsideVerticalFuncSig x =
  HsType' HsTypeForFuncSig HsTypeVertical x

pattern HsTypeInsideDeclSig :: HsType GhcPs -> HsType'
pattern HsTypeInsideDeclSig x = HsType' HsTypeForDeclSig HsTypeNoDir x

newtype StmtLRInsideVerticalList =
  StmtLRInsideVerticalList (StmtLR GhcPs GhcPs (LHsExpr GhcPs))

newtype ParStmtBlockInsideVerticalList =
  ParStmtBlockInsideVerticalList (ParStmtBlock GhcPs GhcPs)

newtype DeclSig =
  DeclSig (Sig GhcPs)

newtype TopLevelTyFamInstDecl =
  TopLevelTyFamInstDecl (TyFamInstDecl GhcPs)
#if MIN_VERSION_ghc_lib_parser(9,4,1)
-- | A wrapper type for type class constraints; e.g., (Eq a, Ord a) of (Eq
-- a, Ord a) => [a] -> [a]. Either 'HorizontalContext' or 'VerticalContext'
-- is used internally.
newtype Context =
  Context (LHsContext GhcPs)

-- | A wrapper type for printing a context horizontally.
newtype HorizontalContext =
  HorizontalContext (LHsContext GhcPs)

-- | A wrapper type for printing a context vertically.
newtype VerticalContext =
  VerticalContext (LHsContext GhcPs)
#else
-- | A wrapper type for type class constraints; e.g., (Eq a, Ord a) of (Eq
-- a, Ord a) => [a] -> [a]. Either 'HorizontalContext' or 'VerticalContext'
-- is used internally.
newtype Context =
  Context (Maybe (LHsContext GhcPs))

-- | A wrapper type for printing a context horizontally.
newtype HorizontalContext =
  HorizontalContext (Maybe (LHsContext GhcPs))

-- | A wrapper type for printing a context vertically.
newtype VerticalContext =
  VerticalContext (Maybe (LHsContext GhcPs))
#endif
-- | A wrapper type for pretty-printing a value of @ModuleName@ with the
-- @module @ prefix.
--
-- Pretty-printing it via @(string "module " >> pretty (name ::
-- ModuleName))@ locates comments before @name@ in the same line as @module
-- @ and the name will be in the next line. This type is to avoid the
-- problem.
newtype ModuleNameWithPrefix =
  ModuleNameWithPrefix ModuleName

-- | A wrapper for 'LPat' inside a pattern declaration. Here, all infix
-- patterns have extra spaces around the operators, like x : xs.
newtype PatInsidePatDecl =
  PatInsidePatDecl (Pat GhcPs)

newtype LambdaCase =
  LambdaCase (MatchGroup GhcPs (LHsExpr GhcPs))
#if MIN_VERSION_ghc_lib_parser(9,4,1)
newtype ModuleDeprecatedPragma =
  ModuleDeprecatedPragma (WarningTxt GhcPs)
#else
newtype ModuleDeprecatedPragma =
  ModuleDeprecatedPragma WarningTxt
#endif
-- | Use this type to pretty-print a list comprehension.
data ListComprehension =
  ListComprehension
    { listCompLhs :: ExprLStmt GhcPs -- ^ @f x@ of @[f x| x <- xs]@.
    , listCompRhs :: [ExprLStmt GhcPs] -- ^ @x <- xs@ of @[f x| x <- xs]@.
    }

-- | Use this type to pretty-print a do expression.
data DoExpression =
  DoExpression
    { doStmts :: [ExprLStmt GhcPs]
    , doOrMdo :: DoOrMdo
    }

-- | Use this type to pretty-print a @let ... in ...@ expression.
data LetIn =
  LetIn
    { letBinds :: HsLocalBinds GhcPs
    , inExpr   :: LHsExpr GhcPs
    }

-- | Comments belonging to an AST node.
data NodeComments =
  NodeComments
    { commentsBefore    :: [LEpaComment]
    , commentOnSameLine :: Maybe LEpaComment
    , commentsAfter     :: [LEpaComment]
    }

data DoOrMdo
  = Do
  | Mdo

data GRHSExprType
  = GRHSExprNormal
  | GRHSExprCase
  | GRHSExprMultiWayIf
  | GRHSExprLambda
  deriving (Eq)

data GRHSProcType
  = GRHSProcCase
  | GRHSProcLambda

data HsTypeFor
  = HsTypeForNormalDecl
  | HsTypeForInstDecl
  | HsTypeForFuncSig
  | HsTypeForDeclSig

data HsTypeDir
  = HsTypeNoDir
  | HsTypeVertical
