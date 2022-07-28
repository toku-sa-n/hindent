module HIndent.Pretty.TypeClass
  ( Pretty(..)
  ) where

import           HIndent.Types

class Pretty a where
  pretty :: a -> Printer ()
