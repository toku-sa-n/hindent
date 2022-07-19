-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!! DELETE THIS MODULE AFTER THE SUCCESSFUL SWITCH !!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--
-- Note that `haskell-src-exts`' source span is exclusive while
-- `ghc-lib-parser`s one is inclusive.
module ConvertModule
  ( convertModule
  ) where

import           Generics.SYB.Schemes
import qualified GHC.Hs                     as Ext
import qualified GHC.Hs                     as GLP
import qualified GHC.Types.SrcLoc           as Ext
import qualified GHC.Unit                   as Ext
import qualified Language.Haskell.Exts      as HSE
import qualified SwitchToGhcLibParserHelper as Helper

convertModule :: GLP.HsModule -> HSE.Module HSE.SrcSpanInfo
convertModule m = HSE.Module (fullSpan m) (moduleHead m) pragmas imports decls
  where
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

moduleHead :: GLP.HsModule -> Maybe (HSE.ModuleHead HSE.SrcSpanInfo)
moduleHead GLP.HsModule { GLP.hsmodName = Just hsmodName
                        , GLP.hsmodExports = Just hsmodExports
                        } =
  Just $ HSE.ModuleHead headSpan moduleName Nothing exportList
  where
    headSpan =
      HSE.SrcSpanInfo
        (HSE.SrcSpan
           (HSE.srcSpanFilename moduleNameSpan)
           (HSE.srcSpanStartLine moduleNameSpan)
           1
           (HSE.srcSpanEndLine headSpanEnd)
           (HSE.srcSpanEndColumn headSpanEnd + 6 + 1) -- +6 for the trailing " where" and +1 becuase `SrcSpan` excludes the last character.
         )
        []
    headSpanEnd = Helper.convertSrcSpan $ Ext.locA $ Ext.getLoc hsmodExports
    moduleName = HSE.ModuleName (HSE.SrcSpanInfo moduleNameSpan []) (Ext.moduleNameString $ Ext.unLoc hsmodName)
    moduleNameSpan = Helper.convertSrcSpan $ Ext.locA $ Ext.getLoc hsmodName
    exportList = undefined
moduleHead _ = Nothing
