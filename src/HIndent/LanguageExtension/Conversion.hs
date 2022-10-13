{-# LANGUAGE CPP #-}

-- | Operations for converting extensions types.
module HIndent.LanguageExtension.Conversion
  ( getExtensions
  , glpExtensionToCabalExtension
  , uniqueExtensions
  , convertExtension
  , defaultExtensions
  ) where

import           Data.List
import           Data.Text                                          hiding
                                                                    (filter,
                                                                     foldr)
import qualified Data.Text                                          as T
import qualified GHC.LanguageExtensions                             as GLP
import           HIndent.Read
import           HIndent.Types
import qualified Language.Haskell.Extension                         as Cabal
import qualified Language.Haskell.GhclibParserEx.GHC.Driver.Session as GLP
import           Text.Read

-- | Consume an extensions list from arguments.
getExtensions :: [Text] -> [Cabal.Extension]
getExtensions = foldr (f . T.unpack) defaultExtensions
  where
    f "Haskell98" _ = []
    f ('N':'o':x) a
      | Just x' <- readExtension x = delete x' a
    f x a
      | Just x' <- readExtension x = x' : delete x' a
    f x _ = error $ "Unknown extension: " ++ x

-- | Converts a value of the type 'Extension' defined in the
-- 'ghc-lib-parser' package to the same value of the type 'Extension'
-- defined in the 'Cabal' package.
--
-- If 'Cabal' does not support the given extension, it returns a 'Nothing'.
glpExtensionToCabalExtension :: GLP.Extension -> Maybe Cabal.Extension
glpExtensionToCabalExtension = fmap Cabal.EnableExtension . readMaybe . show

-- | This function converts each value of the type 'Extension' defined in
-- the package 'Cabal' in the list to the same value of the type
-- 'Extension' defined in the package 'ghc-lib-parser'.
--
-- If the extension has the 'No' suffix, the extension is removed from the
-- result. If both extensions having and not having the suffix exist in the
-- list, only the most backward one has the effect.
--
-- If converting an extension fails due to neither GHC nor 'ghc-lib-parser'
-- not supporting, or deprecation or removal, the extension is ignored.
uniqueExtensions :: [Cabal.Extension] -> [GLP.Extension]
uniqueExtensions [] = []
uniqueExtensions ((Cabal.EnableExtension e):xs)
  | Just e' <- convertExtension e = e' : uniqueExtensions xs
  | otherwise = uniqueExtensions xs
uniqueExtensions ((Cabal.DisableExtension e):xs) =
  uniqueExtensions $ filter (/= readOrFail (show $ Cabal.EnableExtension e)) xs
uniqueExtensions ((Cabal.UnknownExtension s):_) =
  error $ "Unknown extension: " ++ s

-- | This function converts a value of 'KnownExtension' defined in the
-- 'Cabal' package to the same value of 'Extension' defined in
-- 'ghc-lib-parser'.
--
-- This function returns a 'Just' value if it succeeds in converting.
-- Otherwise (e.g., neigher GHC nor 'ghc-lib-parser' does not the passed
-- extension, or it is deprecated or removed), it returns a 'Nothing'.
convertExtension :: Cabal.KnownExtension -> Maybe GLP.Extension
convertExtension = GLP.readExtension . show

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
