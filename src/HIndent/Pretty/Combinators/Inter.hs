module HIndent.Pretty.Combinators.Inter
  ( inter
  , spaced
  , lined
  , commaSeparated
  ) where

import           Data.List
import           HIndent.Pretty.Combinators.String
import           HIndent.Types

inter :: Printer () -> [Printer ()] -> Printer ()
inter separator = sequence_ . intersperse separator

spaced :: [Printer ()] -> Printer ()
spaced = inter space

lined :: [Printer ()] -> Printer ()
lined = inter newline

commaSeparated :: [Printer ()] -> Printer ()
commaSeparated = inter (string ", ")
