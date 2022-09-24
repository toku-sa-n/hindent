{-# LANGUAGE CPP #-}

module HIndent.Language
  ( convertLanguage
  ) where

import qualified GHC.Driver.Session         as GLP
import qualified Language.Haskell.Extension as Cabal

convertLanguage :: Cabal.Language -> GLP.Language
convertLanguage Cabal.Haskell98           = GLP.Haskell98
convertLanguage Cabal.Haskell2010         = GLP.Haskell2010
#if MIN_VERSION_Cabal(3,6,0)
convertLanguage Cabal.GHC2021             = GLP.GHC2021
#endif
convertLanguage (Cabal.UnknownLanguage s) = error $ "Unknown language: " ++ s
