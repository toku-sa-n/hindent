module HIndent.Pretty.Combinators.Lineup
  ( hTuple
  , vTuple
  , vList
  , vFields
  ) where

import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Combinators.Indent
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
vTuple = vLineup ("( ", ")")

-- | Prints like [ a
--               , b
--               , c
--               ]
vList :: [Printer ()] -> Printer ()
vList = vLineup ("[ ", "]")

-- | Prints like { a
--               , b
--               , c
--               }
vFields :: [Printer ()] -> Printer ()
vFields = vLineup ("{ ", "}")

-- | Prints elements in vertical with the given prefix and suffix.
vLineup :: (String, String) -> [Printer ()] -> Printer ()
vLineup (prefix, suffix) ps = do
  indentedDependingOnHead (string prefix) $ prefixedLined ", " ps
  newline
  string suffix
