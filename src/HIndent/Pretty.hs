{-# LANGUAGE CPP #-}

-- | Pretty printing.
module HIndent.Pretty
  ( Pretty(..)
  , pretty
  ) where

import HIndent.Printer

class Pretty a where
  pretty' :: a -> Printer ()

pretty :: Pretty a => a -> Printer ()
pretty = pretty'
