-- TODO: Hide `prefixedLined` and `inter`.
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
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Combinators.Indent
import           HIndent.Pretty.Combinators.String
import           HIndent.Pretty.Combinators.Wrap
import           HIndent.Types

-- | Apply 'hTuple' or 'vTuple' appropriately.
tuple :: [Printer ()] -> Printer ()
tuple = (<-|>) <$> hTuple <*> vTuple

-- | Apply 'hTuple' or 'vTuple'' appropriately.
tuple' :: [Printer ()] -> Printer ()
tuple' = (<-|>) <$> hTuple <*> vTuple'

-- | Prints like (a, b, c).
hTuple :: [Printer ()] -> Printer ()
hTuple = parens . hCommaSep

-- | Prints like { a
--               , b
--               , c
--               }
vFields :: [Printer ()] -> Printer ()
vFields = vLineup ("{", "}")

-- | Prints like { a
--               , b
--               , c}
vFields' :: [Printer ()] -> Printer ()
vFields' = vLineup' ("{", "}")

-- | Print like {a, b, c}.
hFields :: [Printer ()] -> Printer ()
hFields = braces . hCommaSep

-- | Prints like ( a
--               , b
--               , c
--               )
vTuple :: [Printer ()] -> Printer ()
vTuple = vLineup ("(", ")")

-- | Prints like ( a
--               , b
--               , c)
vTuple' :: [Printer ()] -> Printer ()
vTuple' = vLineup' ("(", ")")

-- | Prints like [a, b, c]
hList :: [Printer ()] -> Printer ()
hList = brackets . vCommaSep

-- | Prints like [ a
--               , b
--               , c
--               ]
vList :: [Printer ()] -> Printer ()
vList = vLineup ("[", "]")

-- | Prints like '( a, b, c).
hPromotedTuple :: [Printer ()] -> Printer ()
hPromotedTuple = promotedTupleParens . hCommaSep

-- | Prints like '[ a, b, c]
hPromotedList :: [Printer ()] -> Printer ()
hPromotedList = promotedListBrackets . hCommaSep

-- | Prints elements in vertical with the given prefix and suffix.
vLineup :: (String, String) -> [Printer ()] -> Printer ()
vLineup (prefix, suffix) ps =
  indentedDependingOnHead (string prefix >> space) $ do
    vCommaSep ps
    newline
    indentedWithSpace (-(fromIntegral (length prefix) + 1)) $ string suffix

-- | Similar to 'vLineup' but the suffix is on the same line as the last
-- element.
vLineup' :: (String, String) -> [Printer ()] -> Printer ()
vLineup' (prefix, suffix) ps =
  indentedDependingOnHead (string prefix >> space) $ do
    vCommaSep ps
    string suffix

spaced :: [Printer ()] -> Printer ()
spaced = inter space

lined :: [Printer ()] -> Printer ()
lined = inter newline

blanklined :: [Printer ()] -> Printer ()
blanklined = inter blankline

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

-- | Prints each element after a space like ' a b c'
spacePrefixed :: [Printer ()] -> Printer ()
spacePrefixed = mapM_ (space >>)

-- | Prints each element after a new line.
newlinePrefixed :: [Printer ()] -> Printer ()
newlinePrefixed = mapM_ (newline >>)

prefixedLined :: String -> [Printer ()] -> Printer ()
prefixedLined _ [] = return ()
prefixedLined pref (x:xs) = do
  x
  indentedWithSpace (fromIntegral (length pref * (-1))) $
    forM_ xs $ \p -> do
      newline
      indentedDependingOnHead (string pref) p

inter :: Printer () -> [Printer ()] -> Printer ()
inter separator = sequence_ . intersperse separator
