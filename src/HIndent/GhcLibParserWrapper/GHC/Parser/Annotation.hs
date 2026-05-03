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
srcSpanAnnAToEpAnn :: SrcSpanAnnA -> SrcSpanAnnA
srcSpanAnnAToEpAnn = id

srcSpanAnnLToEpAnn :: SrcSpanAnnL -> SrcSpanAnnL
srcSpanAnnLToEpAnn = id

srcSpanAnnNToEpAnn :: SrcSpanAnnN -> SrcSpanAnnN
srcSpanAnnNToEpAnn = id

srcSpanAnnPToEpAnn :: SrcSpanAnnP -> SrcSpanAnnP
srcSpanAnnPToEpAnn = id

srcSpanAnnCToEpAnn :: SrcSpanAnnC -> SrcSpanAnnC
srcSpanAnnCToEpAnn = id
#else
srcSpanAnnAToEpAnn :: SrcSpanAnnA -> EpAnn AnnListItem
srcSpanAnnAToEpAnn = ann

srcSpanAnnLToEpAnn :: SrcSpanAnnL -> EpAnn AnnList
srcSpanAnnLToEpAnn = ann

srcSpanAnnNToEpAnn :: SrcSpanAnnN -> EpAnn NameAnn
srcSpanAnnNToEpAnn = ann

srcSpanAnnPToEpAnn :: SrcSpanAnnP -> EpAnn AnnPragma
srcSpanAnnPToEpAnn = ann

srcSpanAnnCToEpAnn :: SrcSpanAnnC -> EpAnn AnnContext
srcSpanAnnCToEpAnn = ann
#endif
