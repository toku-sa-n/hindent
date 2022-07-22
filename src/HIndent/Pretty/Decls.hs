module HIndent.Pretty.Decls
  ( outputDecls
  ) where

import           Control.Monad
import           GHC.Hs
import           HIndent.Pretty.Combinators
import           HIndent.Types

outputDecls :: HsModule -> Printer ()
outputDecls m =
  forM_ (hsmodDecls m) $ \x -> do
    newline
    outputOutputable x
