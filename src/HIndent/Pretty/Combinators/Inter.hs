module HIndent.Pretty.Combinators.Inter
  ( inter
  , spaced
  , lined
  , barSeparated
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

-- | Prints like 'a | b | c'.
barSeparated :: [Printer ()] -> Printer ()
barSeparated = inter (string " | ")

commaSeparated :: [Printer ()] -> Printer ()
commaSeparated = inter (string ", ")
