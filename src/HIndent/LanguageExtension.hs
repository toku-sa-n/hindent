{-# LANGUAGE ScopedTypeVariables #-}

-- | Operations related to language extensions.
module HIndent.LanguageExtension
  ( implicitExtensions
  , extensionImplies
  , collectLanguageExtensionsFromSource
  ) where

import           Data.Char
import           Data.List.Split
import           Data.Maybe
import qualified GHC.Driver.Session                   as GLP
import           HIndent.LanguageExtension.Conversion
import qualified HIndent.LanguageExtension.Conversion as EC
import           HIndent.Pragma
import           HIndent.Read
import qualified Language.Haskell.Extension           as Cabal
import           Text.Read
import           Text.Regex.TDFA

-- | This function returns a list of extensions that the passed language
-- (e.g., GHC2021) enables.
implicitExtensions :: GLP.Language -> [Cabal.Extension]
implicitExtensions =
  mapMaybe glpExtensionToCabalExtension . GLP.languageExtensions . Just

-- | This function returns a list of extensions that the passed extension
-- enables.
--
-- For example, "GADTs" enables "GADTSyntax".
extensionImplies :: Cabal.Extension -> [Cabal.Extension]
extensionImplies (Cabal.EnableExtension e) =
  toExtension <$>
  filter (\(a, _, _) -> EC.convertExtension e == Just a) GLP.impliedXFlags
  where
    toExtension (_, True, e')  = Cabal.EnableExtension $ readOrFail $ show e'
    toExtension (_, False, e') = Cabal.DisableExtension $ readOrFail $ show e'
extensionImplies _ = []

collectLanguageExtensionsFromSource :: String -> [Cabal.Extension]
collectLanguageExtensionsFromSource =
  (++) <$> collectLanguageExtensionsSpecifiedViaLanguagePragma <*>
  collectLanguageExtensionsFromSourceViaOptionsPragma

-- | Collects language extensions enabled or disabled by @{-# LANGUAGE FOO
-- #-}@.
--
-- This function ignores language extensions not supported by Cabal.
collectLanguageExtensionsSpecifiedViaLanguagePragma ::
     String -> [Cabal.Extension]
collectLanguageExtensionsSpecifiedViaLanguagePragma =
  mapMaybe (strToExt . stripSpaces) .
  concatMap (splitOn ",") .
  fmap snd . filter ((== "LANGUAGE") . fst) . extractPragmasFromCode

-- | Extracts the language extensions specified by @-XFOO@ from @OPTIONS@
-- or @OPTIONS_GHC@ pragmas
collectLanguageExtensionsFromSourceViaOptionsPragma ::
     String -> [Cabal.Extension]
collectLanguageExtensionsFromSourceViaOptionsPragma =
  mapMaybe (strToExt . stripSpaces) .
  concatMap extractLanguageExtensionsFromOptions .
  fmap snd .
  filter ((`elem` ["OPTIONS", "OPTIONS_GHC"]) . fst) . extractPragmasFromCode

-- | Extracts the language extensions specified in the '-XFOO' format from
-- the given string
extractLanguageExtensionsFromOptions :: String -> [String]
extractLanguageExtensionsFromOptions options =
  fmap
    trimXOption
    (getAllTextMatches (options =~ "-X[^,[:space:]]+") :: [String])
  where
    trimXOption ('-':'X':xs) = xs
    trimXOption _ = error "Unreachable: the option must have the `-X` prefix."

-- | Removes spaces before and after the string.
stripSpaces :: String -> String
stripSpaces = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Converts the given string to an extension, or returns a 'Nothing' on
-- fail.
strToExt :: String -> Maybe Cabal.Extension
strToExt ('N':'o':s) = Cabal.DisableExtension <$> readMaybe s
strToExt s           = Cabal.EnableExtension <$> readMaybe s
