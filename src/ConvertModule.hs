-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!! DELETE THIS MODULE AFTER THE SUCCESSFUL SWITCH !!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--
-- Note that `haskell-src-exts`' source span is exclusive while
-- `ghc-lib-parser`s one is inclusive.
module ConvertModule
  ( convertModule
  ) where

import qualified GHC.Hs                     as GLP
import qualified Language.Haskell.Exts      as HSE

convertModule :: GLP.HsModule -> HSE.Module HSE.SrcSpanInfo
convertModule=undefined
