-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!! DELETE THIS MODULE AFTER THE SUCCESSFUL SWITCH !!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module ConvertModule
  ( convertModule
  ) where

import qualified GHC.Hs                as GLP
import qualified Language.Haskell.Exts as HSE

convertModule :: GLP.HsModule -> HSE.Module HSE.SrcSpanInfo
convertModule m = HSE.Module fullSpan moduleHead pragmas imports decls
  where
    fullSpan = undefined
    moduleHead = undefined
    pragmas = undefined
    imports = undefined
    decls = undefined
