{-# LANGUAGE ScopedTypeVariables #-}

-- | Operations related to language extensions.
module HIndent.Extension
  ( implicitExtensions
  , extensionImplies
  , collectLanguageExtensionsFromSource
  ) where

import           Data.List.Split
import           Data.Maybe
import qualified GHC.Driver.Session           as GLP
import           HIndent.Extension.Conversion
import qualified HIndent.Extension.Conversion as EC
import           HIndent.Read
import qualified Language.Haskell.Extension   as Cabal
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

-- | Collects language extensions enabled or disabled by @{-# LANGUAGE FOO
-- #-}@.
--
-- This function ignores language extensions not supported by Cabal.
collectLanguageExtensionsFromSource :: String -> [Cabal.Extension]
collectLanguageExtensionsFromSource = concatMap lineToExt . lines
  where
    lineToExt :: String -> [Cabal.Extension]
    lineToExt l
      | (_, _, _, exts) :: (String, String, String, [String]) <- l =~ regex =
        mapMaybe strToExt $ concatMap (splitOn ",") exts
    strToExt ('N':'o':s) = Cabal.DisableExtension <$> readMaybe s
    strToExt s           = Cabal.EnableExtension <$> readMaybe s
    regex = "{-# +LANGUAGE +([a-zA-Z0-9-,]+) +#-}"
