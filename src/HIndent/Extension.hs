module HIndent.Extension
  ( implicitExtensions
  , extensionImplies
  ) where

import qualified GHC.Driver.Session           as GLP
import           HIndent.Extension.Conversion
import qualified HIndent.Extension.Conversion as EC
import           HIndent.Read
import qualified Language.Haskell.Extension   as Cabal

-- | This function returns a list of extensions that the passed language
-- (e.g., GHC2021) enables.
implicitExtensions :: GLP.Language -> [Cabal.Extension]
implicitExtensions =
  fmap glpExtensionToCabalExtension . GLP.languageExtensions . Just

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
