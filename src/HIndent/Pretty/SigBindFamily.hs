-- | A module defining 'SigMethodFamily' and other related types and
-- functions.
module HIndent.Pretty.SigBindFamily
  ( SigBindFamily(..)
  , LSigBindFamily
  ) where

import           GHC.Hs
import           GHC.Types.SrcLoc

-- | A sum type containing one of those: 'Sig', 'HsBindLR', and
-- 'FamilyDecl'.
data SigBindFamily
  = Sig (Sig GhcPs)
  | Bind (HsBindLR GhcPs GhcPs)
  | TypeFamily (FamilyDecl GhcPs)

-- | 'SigBindFamily' with the location information.
type LSigBindFamily = GenLocated SrcSpanAnnA SigBindFamily
