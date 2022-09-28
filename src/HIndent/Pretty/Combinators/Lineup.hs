-- | Printer combinators for lining up multiple elements.
module HIndent.Pretty.Combinators.Lineup
  ( tuple
  , tuple'
  , hTuple
  , hFields
  , vFields
  , vFields'
  , vTuple
  , vTuple'
  , hList
  , vList
  , hPromotedTuple
  , hPromotedList
  , spaced
  , lined
  , blanklined
  , barSep
  , hBarSep
  , vBarSep
  , commaSep
  , hCommaSep
  , vCommaSep
  , spacePrefixed
  , newlinePrefixed
  , prefixedLined
  , inter
  ) where

import           Control.Monad
import           Data.List
import           HIndent.Pretty.Combinators.Indent
import           HIndent.Pretty.Combinators.String
import           HIndent.Pretty.Combinators.Switch
import           HIndent.Pretty.Combinators.Wrap
import           HIndent.Types

-- | Applies 'hTuple' if the result fits in a line or 'vTuple' otherwise.
tuple :: [Printer ()] -> Printer ()
tuple = (<-|>) <$> hTuple <*> vTuple

-- | Applies 'hTuple'' if the result fits in a line or 'vTuple'' otherwise.
tuple' :: [Printer ()] -> Printer ()
tuple' = (<-|>) <$> hTuple <*> vTuple'

-- | Runs printers to construct a tuple in a line.
hTuple :: [Printer ()] -> Printer ()
hTuple = parens . hCommaSep

-- | Runs printers to construct a tuple where elements are aligned
-- vertically.
vTuple :: [Printer ()] -> Printer ()
vTuple = vLineup ("(", ")")

-- | Similar to 'vTuple', but the closing parenthesis is in the last
-- element.
vTuple' :: [Printer ()] -> Printer ()
vTuple' = vLineup' ("(", ")")

-- | Runs printers to construct a record in a line.
hFields :: [Printer ()] -> Printer ()
hFields = braces . hCommaSep

-- | Runs printers to construct a record where elements are aligned
-- vertically.
vFields :: [Printer ()] -> Printer ()
vFields = vLineup ("{", "}")

-- | Similar to 'vFields', but the closing brace is in the same line as the
-- last element.
vFields' :: [Printer ()] -> Printer ()
vFields' = vLineup' ("{", "}")

-- | Runs printers to construct a list in a line.
hList :: [Printer ()] -> Printer ()
hList = brackets . vCommaSep

-- | Runs printers to construct a list where elements are aligned
-- vertically.
vList :: [Printer ()] -> Printer ()
vList = vLineup ("[", "]")

-- | Runs printers to construct a promoted tuple in a line.
hPromotedTuple :: [Printer ()] -> Printer ()
hPromotedTuple = promotedTupleParens . hCommaSep

-- | Runs printers to construct a promoted list in a line.
hPromotedList :: [Printer ()] -> Printer ()
hPromotedList = promotedListBrackets . hCommaSep

-- | Prints elements in vertical with the given prefix and suffix.
vLineup :: (String, String) -> [Printer ()] -> Printer ()
vLineup (prefix, suffix) ps =
  string prefix >>
  space |=> do
    vCommaSep ps
    newline
    indentedWithSpace (-(fromIntegral (length prefix) + 1)) $ string suffix

-- | Similar to 'vLineup' but the suffix is in the same line as the last
-- element.
vLineup' :: (String, String) -> [Printer ()] -> Printer ()
vLineup' (prefix, suffix) ps =
  string prefix >>
  space |=> do
    vCommaSep ps
    string suffix

-- | Runs printers in a line with a space as the separator.
spaced :: [Printer ()] -> Printer ()
spaced = inter space

-- | Runs printers line by line.
lined :: [Printer ()] -> Printer ()
lined = inter newline

-- | Runs printers with a blank line as the separator.
blanklined :: [Printer ()] -> Printer ()
blanklined = inter blankline

-- | Applies 'hBarSep' if the result fits in a line or 'vBarSep' otherwise.
barSep :: [Printer ()] -> Printer ()
barSep = (<-|>) <$> hBarSep <*> vBarSep

-- | Runs printers in a line with a bar as the separator.
hBarSep :: [Printer ()] -> Printer ()
hBarSep = inter (string " | ")

-- | Runs printers where each line except the first one has @| @ as
-- a prefix.
vBarSep :: [Printer ()] -> Printer ()
vBarSep = prefixedLined "| "

-- | Applies 'hCommaSep' if the result fits in a line or 'vCommaSep'
-- otherwise.
commaSep :: [Printer ()] -> Printer ()
commaSep = (<-|>) <$> hCommaSep <*> vCommaSep

-- | Runs printers in a line with a comma as the separator.
hCommaSep :: [Printer ()] -> Printer ()
hCommaSep = inter (string ", ")

-- | Runs printers with each line except the first one has @, @ as
-- a prefix.
vCommaSep :: [Printer ()] -> Printer ()
vCommaSep = prefixedLined ", "

-- | Prints each element after a space like.
spacePrefixed :: [Printer ()] -> Printer ()
spacePrefixed = mapM_ (space >>)

-- | Prints each element after a new line.
newlinePrefixed :: [Printer ()] -> Printer ()
newlinePrefixed = mapM_ (newline >>)

-- | Runs printers with a prefix. The prefix is printed before the indent.
prefixedLined :: String -> [Printer ()] -> Printer ()
prefixedLined _ [] = return ()
prefixedLined pref (x:xs) = do
  x
  indentedWithSpace (fromIntegral $ negate $ length pref) $
    forM_ xs $ \p -> do
      newline
      string pref |=> p

-- Inserts the first printer between each element of the list passed as the
-- second argument and runs them.
inter :: Printer () -> [Printer ()] -> Printer ()
inter separator = sequence_ . intersperse separator
