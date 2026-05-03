{-# LANGUAGE CPP #-}

-- | Pretty printing.
module HIndent.Pretty
  ( Pretty(..)
  ) where

import HIndent.Printer

class Pretty a where
  pretty :: a -> Printer ()
