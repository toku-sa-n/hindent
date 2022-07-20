{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pretty printing.
module HIndent.Pretty
  ( pretty
  ) where

import           GHC.Hs
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Pragma
import           HIndent.Types

-- | Pretty print including comments.
pretty :: HsModule -> Printer ()
pretty m = do
  printPragmasToPrinter m
  printOutputableToPrinter m
