-- | Printer operators for wrapping texts with a prefix and a suffix.
module HIndent.Pretty.Combinators.Wrap
  ( parens
  , parensIfSymbol
  , braces
  , brackets
  , tick
  , tickIfNotSymbol
  , wrapWithBars
  , promotedListBrackets
  , promotedTupleParens
  ) where

import           GHC.Types.Name
import           HIndent.Pretty.Combinators.Indent
import           HIndent.Pretty.Combinators.String
import           HIndent.Types

-- | This function wraps the printer with parentheses.
parens :: Printer a -> Printer a
parens = wrap "(" ")"

-- | This function wraps the printer with parentheses if the identifier
-- contains only symbols.
parensIfSymbol :: OccName -> Printer a -> Printer a
parensIfSymbol name
  | isSymOcc name = parens
  | otherwise = id

-- | This function wraps the printer with braces.
braces :: Printer a -> Printer a
braces = wrap "{" "}"

brackets :: Printer a -> Printer a
brackets = wrap "[" "]"

tick :: Printer a -> Printer a
tick = wrap "`" "`"

tickIfNotSymbol :: OccName -> Printer a -> Printer a
tickIfNotSymbol name
  | isSymOcc name = id
  | otherwise = tick

wrapWithBars :: Printer a -> Printer a
wrapWithBars = wrap "|" "|"

promotedListBrackets :: Printer a -> Printer a
promotedListBrackets = wrap "'[ " "]"

promotedTupleParens :: Printer a -> Printer a
promotedTupleParens = wrap "'( " ")"

wrap :: String -> String -> Printer a -> Printer a
wrap open close p = string open |=> p <* string close
