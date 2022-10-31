module HIndent.Pretty.Combinators.RhsSeparator
  ( rhsSeparator
  ) where

import           HIndent.Pretty.Combinators.String
import           HIndent.Pretty.Types
import           HIndent.Types

rhsSeparator :: GRHSType -> Printer ()
rhsSeparator GRHSNormal = string "="
rhsSeparator GRHSCase   = string "->"
