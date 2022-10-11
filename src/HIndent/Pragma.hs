module HIndent.Pragma
  ( collectPragmaNameAndElement
  , pragmaRegex
  ) where

import           Text.Regex.TDFA

-- | Collects pragmas from the given 'String'.
collectPragmaNameAndElement :: String -> Maybe (String, String) -- ^ The first element is pragma's name (e.g., @"LANGUAGE"@), and the second one is pragma's elements (e.g., @["CPP", "PatternSynonyms"]@).
collectPragmaNameAndElement l
  | (_, _, _, [name, element]) <-
     l =~ pragmaRegex :: (String, String, String, [String]) =
    Just (name, element)
collectPragmaNameAndElement _ = Nothing

-- | A regex to match against a pragma.
pragmaRegex :: String
pragmaRegex = "{-#[[:space:]]+([^[:space:]]+)[[:space:]]+([^#]+)[[:space:]]+#-}"
