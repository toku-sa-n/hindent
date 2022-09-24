module HIndent.Extension
  ( implicitExtensions
  , extensionImplies
  ) where

import qualified GHC.Driver.Session           as GLP
import qualified HIndent.Extension.Conversion as EC
import           HIndent.Read
import qualified Language.Haskell.Extension   as Cabal

implicitExtensions :: GLP.Language -> [Cabal.Extension]
implicitExtensions =
  fmap (Cabal.EnableExtension . readOrFail . show) .
  GLP.languageExtensions . Just

extensionImplies :: Cabal.Extension -> [Cabal.Extension]
extensionImplies (Cabal.EnableExtension e) =
  toExtension <$>
  filter (\(a, _, _) -> Just a == EC.convertExtension e) GLP.impliedXFlags
  where
    toExtension (_, True, e')  = Cabal.EnableExtension $ read $ show e'
    toExtension (_, False, e') = Cabal.DisableExtension $ read $ show e'
extensionImplies _ = []
