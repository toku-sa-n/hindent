module HIndent.Pragma
  ( extractPragmasFromCode
  , extractPragmaNameAndElement
  , pragmaRegex
  ) where

import           Data.Maybe
import           Text.Regex.TDFA

-- | Extracts all pragmas from the given source code.
extractPragmasFromCode :: String -> [(String, String)] -- ^ [(Pragma's name (e.g., @"LANGUAGE"@), Pragma's element (e.g., @"CPP, DerivingVia"@))]
extractPragmasFromCode src =
  mapMaybe
    extractPragmaNameAndElement
    (getAllTextMatches (match pragmaRegex src) :: [String])

-- | Extracts the pragma's name and its element from the given pragma.
--
-- This function returns a 'Nothing' if it fails to extract them.
extractPragmaNameAndElement :: String -> Maybe (String, String) -- ^ [(Pragma's name (e.g., @"LANGUAGE"@), Pragma's element (e.g., @"CPP, DerivingVia"@))]
extractPragmaNameAndElement l
  | (_, _, _, [name, element]) <-
     match pragmaRegex l :: (String, String, String, [String]) =
    Just (name, element)
extractPragmaNameAndElement _ = Nothing

-- | A regex to match against a pragma.
pragmaRegex :: Regex
pragmaRegex =
  makeRegexOpts
    compOption
    execOption
    "{-#[[:space:]]*([^[:space:]]+)[[:space:]]+([^#]+)#-}"

-- | The option for matching against a pragma.
execOption :: ExecOption
execOption = ExecOption {captureGroups = True}

-- | The option for matching against a pragma.
--
-- 'multiline' is set to 'False' to match against multiline pragmas, e.g.,
-- @{-# LANGUAGE CPP\nOverloadedStrings #-}@.
compOption :: CompOption
compOption =
  CompOption
    { caseSensitive = True
    , multiline = False
    , rightAssoc = True
    , newSyntax = True
    , lastStarGreedy = True
    }
