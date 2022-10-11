module HIndent.Pragma
  ( pragmaRegex
  ) where

-- | A regex to match against a pragma.
pragmaRegex :: String
pragmaRegex = "{-#[[:space:]]+([^[:space:]]+)[[:space:]]+([^#]+)[[:space:]]+#-}"
