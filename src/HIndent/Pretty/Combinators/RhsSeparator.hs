module HIndent.Pretty.Combinators.RhsSeparator
  ( rhsSeparator
  ) where

import           HIndent.Pretty.Combinators.String
import           HIndent.Pretty.Types
import           HIndent.Types

rhsSeparator :: GRHSExprType -> Printer ()
rhsSeparator GRHSExprNormal     = string "="
rhsSeparator GRHSExprCase       = string "->"
rhsSeparator GRHSExprMultiWayIf = string "->"
rhsSeparator GRHSExprLambda     = string "->"
