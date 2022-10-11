module HIndent.Pragma
  ( extractPragmaNameAndElement
  , pragmaRegex
  ) where

import           Text.Regex.TDFA

-- | Extracts the pragma's name and its element from the given pragma.
--
-- This function returns a 'Nothing' if it fails to extract them.
extractPragmaNameAndElement :: String -> Maybe (String, String) -- ^ The first element is pragma's name (e.g., @"LANGUAGE"@), and the second one is pragma's elements (e.g., @["CPP", "PatternSynonyms"]@).
extractPragmaNameAndElement l
  | (_, _, _, [name, element]) <-
     l =~ pragmaRegex :: (String, String, String, [String]) =
    Just (name, element)
extractPragmaNameAndElement _ = Nothing

-- | A regex to match against a pragma.
pragmaRegex :: String
pragmaRegex = "{-#[[:space:]]+([^[:space:]]+)[[:space:]]+([^#]+)[[:space:]]+#-}"
