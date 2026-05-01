module HIndent.Ast.Declaration.Bind
  ( prettyBind
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Printer

prettyBind :: GHC.HsBind GHC.GhcPs -> Printer ()
