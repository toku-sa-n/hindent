{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Operations related to language extensions.
module HIndent.LanguageExtension
  ( implicitExtensions
  , extensionImplies
  , collectLanguageExtensionsFromSource
  , defaultExtensions
  , getExtensions
  ) where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Text                            as T
import qualified GHC.Driver.Session                   as GLP
import           HIndent.LanguageExtension.Conversion
import qualified HIndent.LanguageExtension.Conversion as EC
import           HIndent.Pragma
import           HIndent.Read
import           HIndent.Types
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

-- | Collect pragmas specified in the source code.
collectLanguageExtensionsFromSource :: String -> [Cabal.Extension]
collectLanguageExtensionsFromSource =
  (++) <$> collectLanguageExtensionsSpecifiedViaLanguagePragma <*>
  collectLanguageExtensionsFromSourceViaOptionsPragma

-- | Consume an extensions list from arguments.
getExtensions :: [T.Text] -> [Cabal.Extension]
getExtensions = foldr (f . T.unpack) defaultExtensions
  where
    f "Haskell98" _ = []
    f ('N':'o':x) a

      | Just x' <- readExtension x = delete x' a
    f x a

      | Just x' <- readExtension x = x' : delete x' a
    f x _ = error $ "Unknown extension: " ++ x

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

-- | Default extensions.
defaultExtensions :: [Cabal.Extension]
defaultExtensions = fmap Cabal.EnableExtension $ [minBound ..] \\ badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [Cabal.KnownExtension]
badExtensions =
  [ Cabal.Arrows -- steals proc
  , Cabal.TransformListComp -- steals the group keyword
  , Cabal.XmlSyntax
  , Cabal.RegularPatterns -- steals a-b
  , Cabal.UnboxedTuples -- breaks (#) lens operator
  , Cabal.UnboxedSums -- Same as 'UnboxedTuples'
    -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
  , Cabal.PatternSynonyms -- steals the pattern keyword
  , Cabal.RecursiveDo -- steals the rec keyword
  , Cabal.DoRec -- same
  , Cabal.TypeApplications -- since GHC
  , Cabal.StaticPointers -- Steals the `static` keyword
  ] ++
  badExtensionsSinceGhc920 ++ badExtensionsSinceGhc941

-- | Additional disabled extensions since GHC 9.2.0.
badExtensionsSinceGhc920 :: [Cabal.KnownExtension]
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
badExtensionsSinceGhc920 =
  [ Cabal.OverloadedRecordDot -- Breaks 'a.b'
  , Cabal.LexicalNegation -- Cannot handle minus signs in some cases
  ]
#else
badExtensionsSinceGhc920 = []
#endif
-- | Additionally disabled extensions since GHC 9.4.1.
--
-- With these extensions enabled, a few tests fail.
badExtensionsSinceGhc941 :: [Cabal.KnownExtension]
#if MIN_VERSION_GLASGOW_HASKELL(9,4,1,0)
badExtensionsSinceGhc941 =
  [ Cabal.OverloadedRecordUpdate
  , Cabal.AlternativeLayoutRule
  , Cabal.AlternativeLayoutRuleTransitional
  ]
#else
badExtensionsSinceGhc941 = []
#endif
