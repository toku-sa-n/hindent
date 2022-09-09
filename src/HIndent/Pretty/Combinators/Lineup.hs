module HIndent.Pretty.Combinators.Lineup
  ( tuple
  , hTuple
  , hFields
  , vTuple
  , vList
  , vFields
  , spaced
  , lined
  , barSep
  , hBarSep
  , vBarSep
  , commaSep
  , hCommaSep
  , vCommaSep
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
hTuple = parens . hCommaSep

-- | Print like {a, b, c}.
hFields :: [Printer ()] -> Printer ()
hFields = braces . hCommaSep

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
    vCommaSep ps
    newline
    indentedWithSpace (-2) $ string [suffix]

spaced :: [Printer ()] -> Printer ()
spaced = inter space

lined :: [Printer ()] -> Printer ()
lined = inter newline

-- | Apply 'hBarSep' or 'vBarSep' appropriately.
barSep :: [Printer ()] -> Printer ()
barSep = (<-|>) <$> hBarSep <*> vBarSep

-- | Prints like 'a | b | c'.
hBarSep :: [Printer ()] -> Printer ()
hBarSep = inter (string " | ")

-- | Prints like a
--             | b
--             | c
vBarSep :: [Printer ()] -> Printer ()
vBarSep = prefixedLined "| "

-- | Apply 'hCommaSep' or 'vCommaSep'.
commaSep :: [Printer ()] -> Printer ()
commaSep = (<-|>) <$> hCommaSep <*> vCommaSep

-- | Prints like 'a, b, c'.
hCommaSep :: [Printer ()] -> Printer ()
hCommaSep = inter (string ", ")

-- | Prints like a
--             , b
--             , c.
vCommaSep :: [Printer ()] -> Printer ()
vCommaSep = prefixedLined ", "

inter :: Printer () -> [Printer ()] -> Printer ()
inter separator = sequence_ . intersperse separator
