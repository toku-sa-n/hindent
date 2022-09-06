module HIndent.SrcSpan
  ( startLine
  ) where

import           GHC.Types.SrcLoc

startLine :: SrcSpan -> Int
startLine (RealSrcSpan x _) = srcSpanStartLine x
startLine (UnhelpfulSpan _) = error "Ths src span is unavailable."
