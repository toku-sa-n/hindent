module HIndent.Pragma
  ( collectPragmaNameAndElements
  , pragmaRegex
  ) where

import           Data.Char
import           Data.List.Split
import           Text.Regex.TDFA

-- | Collects pragmas from the given 'String'.
collectPragmaNameAndElements :: String -> Maybe (String, [String]) -- ^ The first element is pragma's name (e.g., @"LANGUAGE"@), and the second one is pragma's elements (e.g., @["CPP", "PatternSynonyms"]@).
collectPragmaNameAndElements l
  | (_, _, _, [name, elements]) <-
     l =~ pragmaRegex :: (String, String, String, [String]) =
    Just (name, strip <$> splitOn "," elements)
  where
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
collectPragmaNameAndElements _ = Nothing

-- | A regex to match against a pragma.
pragmaRegex :: String
pragmaRegex = "{-#[[:space:]]+([^[:space:]]+)[[:space:]]+([^#]+)[[:space:]]+#-}"
