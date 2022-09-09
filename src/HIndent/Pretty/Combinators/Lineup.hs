module HIndent.Pretty.Combinators.Lineup
  ( tuple
  , hTuple
  , hFields
  , vTuple
  , vList
  , vFields
  , spaced
  , lined
  , barSeparated
  , commaSeparated
  , inter
  ) where

import           Data.List
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Combinators.Indent
import           HIndent.Pretty.Combinators.String
import           HIndent.Pretty.Combinators.Wrap
import           HIndent.Types

-- | Apply 'hTuple' or 'vTuple' appropriately.
tuple :: [Printer ()] -> Printer ()
tuple = (<-|>) <$> hTuple <*> vTuple

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

spaced :: [Printer ()] -> Printer ()
spaced = inter space

lined :: [Printer ()] -> Printer ()
lined = inter newline

-- | Prints like 'a | b | c'.
barSeparated :: [Printer ()] -> Printer ()
barSeparated = inter (string " | ")

commaSeparated :: [Printer ()] -> Printer ()
commaSeparated = inter (string ", ")

inter :: Printer () -> [Printer ()] -> Printer ()
inter separator = sequence_ . intersperse separator
