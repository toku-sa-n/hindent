module SwitchToGhcLibParserHelper
  ( cabalExtensionToHSEExtension
  , SrcSpanInfo(..)
  , fromHSESrcSpanInfo
  , SrcSpan(..)
  , fromHSESrcSpan
  , toHSESrcSpan
  , toExtension
  ) where

import qualified GHC.LanguageExtensions          as GLE
import qualified Language.Haskell.Extension      as Cabal
import qualified Language.Haskell.Exts           as HSE
import qualified Language.Haskell.Exts.Extension as HSE

cabalExtensionToHSEExtension :: Cabal.Extension -> HSE.Extension
cabalExtensionToHSEExtension = undefined

data SrcSpan =
  SrcSpan
    { srcSpanFilename    :: String
    , srcSpanStartLine   :: Int
    , srcSpanStartColumn :: Int
    , srcSpanEndLine     :: Int
    , srcSpanEndColumn   :: Int
    }
  deriving (Show, Eq)

fromHSESrcSpan :: HSE.SrcSpan -> SrcSpan
fromHSESrcSpan (HSE.SrcSpan name sl sc el ec) = SrcSpan name sl sc el ec

toHSESrcSpan :: SrcSpan -> HSE.SrcSpan
toHSESrcSpan (SrcSpan name sl sc el ec) = HSE.SrcSpan name sl sc el ec

data SrcSpanInfo =
  SrcSpanInfo
    { srcInfoSpan   :: HSE.SrcSpan
    , srcInfoPoints :: [HSE.SrcSpan]
    }

fromHSESrcSpanInfo :: HSE.SrcSpanInfo -> SrcSpanInfo
fromHSESrcSpanInfo (HSE.SrcSpanInfo s p) = SrcSpanInfo s p

toExtension :: GLE.Extension -> HSE.Extension
toExtension = HSE.EnableExtension . read . show
