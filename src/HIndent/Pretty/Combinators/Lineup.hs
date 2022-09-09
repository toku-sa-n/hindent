module HIndent.Pretty.Combinators.Lineup
  ( hTuple
  , vTuple
  , vList
  ) where

import           HIndent.Pretty.Combinators.Inter
import           HIndent.Pretty.Combinators.String
import           HIndent.Pretty.Combinators.Wrap
import           HIndent.Types

-- | Prints like (a, b, c).
hTuple :: [Printer ()] -> Printer ()
hTuple = parens . commaSeparated

-- | Prints like ( a
--               , b
--               , c
--               )
vTuple :: [Printer ()] -> Printer ()
vTuple ps = do
  string "( "
  inter (newline >> string ", ") ps
  newline
  string ")"

vList :: [Printer ()] -> Printer ()
vList ps = do
  string "[ "
  inter (newline >> string ", ") ps
  newline
  string "]"
