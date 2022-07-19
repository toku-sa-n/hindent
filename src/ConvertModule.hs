-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!! DELETE THIS MODULE AFTER THE SUCCESSFUL SWITCH !!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module ConvertModule
  ( convertModule
  ) where

import           Generics.SYB.Schemes
import qualified GHC.Hs                     as GLP
import qualified Language.Haskell.Exts      as HSE
import qualified SwitchToGhcLibParserHelper as Helper

convertModule :: GLP.HsModule -> HSE.Module HSE.SrcSpanInfo
convertModule m = HSE.Module (fullSpan m) moduleHead pragmas imports decls
  where
    moduleHead = undefined
    pragmas = undefined
    imports = undefined
    decls = undefined

fullSpan :: GLP.HsModule -> HSE.SrcSpanInfo
fullSpan m =
  HSE.SrcSpanInfo
    eofPosition {HSE.srcSpanStartLine = 1, HSE.srcSpanStartColumn = 1}
    []
  where
    eofPosition =
      Helper.convertSpan $ GLP.ac_prior_tok $ head $ listify isEofComment m
    isEofComment (GLP.EpaComment GLP.EpaEofComment _) = True
    isEofComment _                                    = False
