module SwitchToGhcLibParserHelper
  ( cabalExtensionToHSEExtension
  ) where

import qualified Language.Haskell.Exts.Extension as HSE
import qualified Language.Haskell.Extension as Cabal

cabalExtensionToHSEExtension :: Cabal.Extension -> HSE.Extension
cabalExtensionToHSEExtension = undefined
