-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!! DELETE THIS MODULE AFTER THE SUCCESSFUL SWITCH !!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module ConvertModule
  ( convertModule
  ) where

import qualified GHC.Hs                as GLP
import qualified Language.Haskell.Exts as HSE

convertModule :: GLP.HsModule -> HSE.Module HSE.SrcSpanInfo
convertModule = undefined
