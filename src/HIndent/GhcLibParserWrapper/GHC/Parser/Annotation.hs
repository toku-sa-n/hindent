{-# LANGUAGE CPP #-}

module HIndent.GhcLibParserWrapper.GHC.Parser.Annotation
  ( module GHC.Parser.Annotation
  , epaLocationToRealSrcSpan
  , srcSpanAnnAToEpAnn
  , srcSpanAnnLToEpAnn
  , srcSpanAnnNToEpAnn
  , srcSpanAnnPToEpAnn
  , srcSpanAnnCToEpAnn
  ) where

import GHC.Parser.Annotation
import GHC.Types.SrcLoc
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
epaLocationToRealSrcSpan :: EpaLocation' a -> RealSrcSpan
epaLocationToRealSrcSpan = epaLocationRealSrcSpan
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
epaLocationToRealSrcSpan :: EpaLocation' a -> RealSrcSpan
epaLocationToRealSrcSpan = anchor
#else
epaLocationToRealSrcSpan :: Anchor -> RealSrcSpan
epaLocationToRealSrcSpan = anchor
#endif

#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
srcSpanAnnAToEpAnn = id
srcSpanAnnLToEpAnn = id
srcSpanAnnNToEpAnn = id
srcSpanAnnPToEpAnn = id
srcSpanAnnCToEpAnn = id
#else
srcSpanAnnAToEpAnn = ann
srcSpanAnnLToEpAnn = ann
srcSpanAnnNToEpAnn = ann
srcSpanAnnPToEpAnn = ann
srcSpanAnnCToEpAnn = ann
#endif
