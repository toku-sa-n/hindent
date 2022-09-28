-- | Printer combinators for printing values of types implementing
-- 'Outputable'.
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

-- | Prints the given value using the type's 'Outputable' implementation.
--
-- The use of this function should be avoided if possible because it may
-- raise an error due to 'showPpr' returning a 'String' containing @\n@s.
output :: (HasCallStack, Outputable a) => a -> Printer ()
output = string . showOutputable

-- | Converts the given value to a 'String'.
showOutputable :: Outputable a => a -> String
showOutputable = showPpr dynFlags

-- | 'DynFlags' for calling 'showPpr'
dynFlags :: DynFlags
dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig
