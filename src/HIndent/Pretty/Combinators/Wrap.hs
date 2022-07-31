module HIndent.Pretty.Combinators.Wrap
  ( parens
  , brackets
  , tick
  , wrapWithBars
  ) where

import           HIndent.Pretty.Combinators
import           HIndent.Types

parens :: Printer a -> Printer a
parens = wrap "(" ")"

brackets :: Printer a -> Printer a
brackets = wrap "[" "]"

tick :: Printer a -> Printer a
tick = wrap "`" "`"

wrapWithBars :: Printer a -> Printer a
wrapWithBars = wrap "|" "|"

wrap :: String -> String -> Printer a -> Printer a
wrap open close p = indentedDependingOnHead (string open) $ p <* string close
