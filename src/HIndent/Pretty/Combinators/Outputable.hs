module HIndent.Pretty.Combinators.Outputable
  ( output
  , showOutputable
  ) where

import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Stack
import           GHC.Utils.Outputable
import           HIndent.Pretty.Combinators.String
import           HIndent.Types
import           Language.Haskell.GhclibParserEx.GHC.Settings.Config

output :: (HasCallStack, Outputable a) => a -> Printer ()
output = string . showOutputable

showOutputable :: Outputable a => a -> String
showOutputable = showPpr dynFlags

dynFlags :: DynFlags
dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig
