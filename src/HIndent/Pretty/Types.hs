{-# LANGUAGE CPP #-}

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
  , MatchGroupForCase(..)
  , MatchGroupForLambda(..)
  , MatchGroupForLambdaInProc(..)
  , MatchForCase(..)
  , MatchForLambda(..)
  , MatchForLambdaInProc(..)
  , GRHSsForCase(..)
  , GRHSsForLambda(..)
  , GRHSsForLambdaInProc(..)
  , GRHSForCase(..)
  , GRHSForMultiwayIf(..)
  , GRHSForLambda(..)
  , GRHSForLambdaInProc(..)
  , RecConPat(..)
  , RecConField(..)
  , HsSigTypeInsideInstDecl(..)
  , HsSigTypeInsideVerticalFuncSig(..)
  , HsSigTypeInsideDeclSig(..)
  , HsTypeInsideInstDecl(..)
  , HsTypeInsideVerticalFuncSig(..)
  , StmtLRInsideVerticalList(..)
  , ParStmtBlockInsideVerticalList(..)
  , DeclSig(..)
  , DeclTypeFamily(..)
  , HsTypeInsideDeclSig(..)
  , HsTypeInsideVerticalDeclSig(..)
  , Context(..)
  , HorizontalContext(..)
  , VerticalContext(..)
  , HsDataDefnForDataInstance(..)
  , ModuleNameWithPrefix(..)
  , PatInsidePatDecl(..)
  , LambdaCase(..)
  , ListComprehension(..)
  , DoExpression(..)
  , DoOrMdo(..)
  , LetIn(..)
  ) where

import           GHC.Hs
import           GHC.Types.Name.Reader
import           GHC.Unit

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

newtype MatchGroupForCase =
  MatchGroupForCase (MatchGroup GhcPs (LHsExpr GhcPs))

newtype MatchGroupForLambda =
  MatchGroupForLambda (MatchGroup GhcPs (LHsExpr GhcPs))

-- | 'MatchGroup' inside a lambda of a @proc@ expression.
newtype MatchGroupForLambdaInProc =
  MatchGroupForLambdaInProc (MatchGroup GhcPs (LHsCmd GhcPs))

newtype MatchForCase =
  MatchForCase (Match GhcPs (LHsExpr GhcPs))

newtype MatchForLambda =
  MatchForLambda (Match GhcPs (LHsExpr GhcPs))

-- | 'Match' for a lambda inside a @proc@ expression.
newtype MatchForLambdaInProc =
  MatchForLambdaInProc (Match GhcPs (LHsCmd GhcPs))

newtype GRHSsForCase =
  GRHSsForCase (GRHSs GhcPs (LHsExpr GhcPs))

newtype GRHSsForLambda =
  GRHSsForLambda (GRHSs GhcPs (LHsExpr GhcPs))

-- | 'GRHSs' for a lambda inside a @proc@ expression.
newtype GRHSsForLambdaInProc =
  GRHSsForLambdaInProc (GRHSs GhcPs (LHsCmd GhcPs))

newtype GRHSForCase =
  GRHSForCase (GRHS GhcPs (LHsExpr GhcPs))

newtype GRHSForMultiwayIf =
  GRHSForMultiwayIf (GRHS GhcPs (LHsExpr GhcPs))

newtype GRHSForLambda =
  GRHSForLambda (GRHS GhcPs (LHsExpr GhcPs))

newtype GRHSForLambdaInProc =
  GRHSForLambdaInProc (GRHS GhcPs (LHsCmd GhcPs))

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

newtype HsTypeInsideInstDecl =
  HsTypeInsideInstDecl (HsType GhcPs)

newtype HsTypeInsideVerticalFuncSig =
  HsTypeInsideVerticalFuncSig (HsType GhcPs)

newtype StmtLRInsideVerticalList =
  StmtLRInsideVerticalList (StmtLR GhcPs GhcPs (LHsExpr GhcPs))

newtype ParStmtBlockInsideVerticalList =
  ParStmtBlockInsideVerticalList (ParStmtBlock GhcPs GhcPs)

newtype DeclSig =
  DeclSig (Sig GhcPs)

-- | Top-level type family declaration, like @type family ID a :: *@.
newtype DeclTypeFamily =
  DeclTypeFamily (FamilyDecl GhcPs)

newtype HsTypeInsideDeclSig =
  HsTypeInsideDeclSig (HsType GhcPs)

newtype HsTypeInsideVerticalDeclSig =
  HsTypeInsideVerticalDeclSig (HsType GhcPs)
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
-- | 'HsDataDefn' for 'data instance'.
newtype HsDataDefnForDataInstance =
  HsDataDefnForDataInstance (HsDataDefn GhcPs)

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

data DoOrMdo
  = Do
  | Mdo
