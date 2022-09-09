module HIndent.Pretty.Combinators.Lineup
  ( hTuple
  , hFields
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

-- | Print like {a, b, c}.
hFields :: [Printer ()] -> Printer ()
hFields = braces . commaSeparated

-- | Prints like ( a
--               , b
--               , c
--               )
vTuple :: [Printer ()] -> Printer ()
vTuple = vLineup ('(', ')')

-- | Prints like [ a
--               , b
--               , c
--               ]
vList :: [Printer ()] -> Printer ()
vList = vLineup ('[', ']')

-- | Prints like { a
--               , b
--               , c
--               }
vFields :: [Printer ()] -> Printer ()
vFields = vLineup ('{', '}')

-- | Prints elements in vertical with the given prefix and suffix.
vLineup :: (Char, Char) -> [Printer ()] -> Printer ()
vLineup (prefix, suffix) ps =
  indentedDependingOnHead (string $ prefix : " ") $ do
    prefixedLined ", " ps
    newline
    indentedWithSpace (-2) $ string [suffix]
