{-# LANGUAGE CPP #-}

module HIndent.Pragma
  ( extractPragmasFromCode
  , extractPragmaNameAndElement
  , pragmaRegex
  ) where

import           Data.Maybe
import           GHC.Data.EnumSet
import           GHC.Data.FastString
import           GHC.Data.StringBuffer
import           GHC.Parser.Lexer
import           GHC.Types.SrcLoc
import           Text.Regex.TDFA       hiding (empty)
#if MIN_VERSION_ghc_lib_parser(9,4,1)
import           GHC.Utils.Error
import           GHC.Utils.Outputable  hiding (empty, text, (<>))
#endif
-- | Extracts all pragmas from the given source code.
--
-- FIXME: The function is slow because it lexicographically analyzes the
-- given source code. An alternative way is to use regular expressions.
-- However, this method cannot determine if what appears to be a pragma is
-- really a pragma, or requires complex regular expressions. For example,
-- @{-\n\n{-# LANGUAGE CPP #-}\n\n-}@ is not a pragma, but is likely to be
-- recognized as such.
extractPragmasFromCode :: String -> [(String, String)] -- ^ [(Pragma's name (e.g., @"LANGUAGE"@), Pragma's element (e.g., @"CPP, DerivingVia"@))]
extractPragmasFromCode =
  mapMaybe extractPragmaNameAndElement .
  mapMaybe extractBlockComment . lexModule
  where
    extractBlockComment (ITblockComment c _) = Just c
    extractBlockComment _                    = Nothing

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
    "^{-#[[:space:]]*([^[:space:]]+)[[:space:]]+([^#]+)#-}"

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

-- TODO: Merge with parsing codes in the 'HIndent' module.
lexModule :: String -> [Token]
lexModule code
  | POk _ tokens <-
     lexTokenStream
       parserOpts
       (stringToStringBuffer code)
       (mkRealSrcLoc (mkFastString "<interactive>") 1 1) = fmap unLoc tokens
  | otherwise = error "Failed to lex the code."

parserOpts :: ParserOpts
#if MIN_VERSION_ghc_lib_parser(9,4,1)
parserOpts = mkParserOpts empty diagOpts [] False False True False
  where
    diagOpts = DiagOpts empty empty False False Nothing defaultSDocContext
#else
parserOpts = mkParserOpts empty empty False False True False
#endif
