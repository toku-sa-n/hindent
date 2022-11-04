{-# LANGUAGE CPP #-}

-- | Operations for converting extensions types.
module HIndent.LanguageExtension.Conversion
  ( glpExtensionToCabalExtension
  , uniqueExtensions
  , convertExtension
  ) where

import qualified GHC.LanguageExtensions                             as GLP
import           HIndent.Read
import qualified Language.Haskell.Extension                         as Cabal
import qualified Language.Haskell.GhclibParserEx.GHC.Driver.Session as GLP
import           Text.Read

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
  | otherwise                     = uniqueExtensions xs
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