-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!! DELETE THIS MODULE AFTER THE SUCCESSFUL SWITCH !!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--
-- Note that `haskell-src-exts`' source span is exclusive while
-- `ghc-lib-parser`s one is inclusive.
module SwitchToGhcLibParserHelper.ConvertModule
  ( convertModule
  ) where

import qualified GHC.Hs                as GLP
import qualified GHC.Types.SrcLoc      as GLP
import qualified Language.Haskell.Exts as HSE

convertModule :: GLP.HsModule -> HSE.Module HSE.SrcSpanInfo
convertModule m = HSE.Module undefined undefined undefined undefined (decls m)

decls :: GLP.HsModule -> [HSE.Decl HSE.SrcSpanInfo]
decls = fmap convertDecl . GLP.hsmodDecls

convertDecl :: GLP.LHsDecl GLP.GhcPs -> HSE.Decl HSE.SrcSpanInfo
convertDecl (GLP.L (GLP.SrcSpanAnn _ loc) decl) =
  case decl of
    GLP.TyClD _ _      -> undefined
    GLP.InstD _ _      -> undefined
    GLP.DerivD _ _     -> undefined
    GLP.ValD _ _       -> undefined
    GLP.SigD _ _       -> undefined
    GLP.KindSigD _ _   -> undefined
    GLP.DefD _ _       -> undefined
    GLP.ForD _ _       -> undefined
    GLP.WarningD _ _   -> undefined
    GLP.AnnD _ _       -> undefined
    GLP.RuleD _ _      -> undefined
    GLP.SpliceD _ _    -> undefined
    GLP.DocD _ _       -> undefined
    GLP.RoleAnnotD _ _ -> undefined
