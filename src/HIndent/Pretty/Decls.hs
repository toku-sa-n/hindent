module HIndent.Pretty.Decls
  ( outputDecls
  , declsExist
  ) where

import           GHC.Hs
import           HIndent.Pretty.Combinators
import           HIndent.Types

outputDecls :: HsModule -> Printer ()
outputDecls = inter newline . fmap outputOutputable . hsmodDecls

declsExist :: HsModule -> Bool
declsExist = not . null . hsmodDecls
