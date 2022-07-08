module SwitchToGhcLibParserHelper
  ( cabalExtensionToHSEExtension
  , SrcSpanInfo(..)
  ) where

import qualified Language.Haskell.Extension      as Cabal
import qualified Language.Haskell.Exts.Extension as HSE
import qualified Language.Haskell.Exts as HSE

cabalExtensionToHSEExtension :: Cabal.Extension -> HSE.Extension
cabalExtensionToHSEExtension = undefined

data SrcSpanInfo =
  SrcSpanInfo
    { srcInfoSpan   :: HSE.SrcSpan
    , srcInfoPoints :: HSE.SrcSpan
    }
