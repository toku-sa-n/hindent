{-# LANGUAGE CPP #-}

module HIndent.CabalFile
  ( getCabalExtensionsForSourcePath
  ) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import Data.Traversable
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
#if MIN_VERSION_Cabal(3, 6, 0)
import Distribution.Utils.Path (getSymbolicPath)
#endif
#if MIN_VERSION_Cabal(2, 2, 0)
import Distribution.PackageDescription.Parsec
#else
import Distribution.PackageDescription.Parse
#endif
import Language.Haskell.Extension
import qualified GHC.LanguageExtensions.Type as GLP
import qualified GHC.Driver.Session as GLP
import GHC.Driver.Session (languageExtensions, impliedXFlags)
import System.Directory
import System.FilePath

data Stanza = MkStanza
  { _stanzaBuildInfo :: BuildInfo
  , stanzaIsSourceFilePath :: FilePath -> Bool
  }

-- | Find the relative path of a child path in a parent, if it is a child
toRelative :: FilePath -> FilePath -> Maybe FilePath
toRelative parent child = let
  rel = makeRelative parent child
  in if rel == child
       then Nothing
       else Just rel

-- | Create a Stanza from `BuildInfo` and names of modules and paths
mkStanza :: BuildInfo -> [ModuleName] -> [FilePath] -> Stanza
mkStanza bi mnames fpaths =
  MkStanza bi $ \path -> let
    modpaths = fmap toFilePath $ otherModules bi ++ mnames
    inDir dir =
      case toRelative dir path of
        Nothing -> False
        Just relpath ->
          any (equalFilePath $ dropExtension relpath) modpaths ||
          any (equalFilePath relpath) fpaths
    in any inDir $ hsSourceDirs' bi
      where
#if MIN_VERSION_Cabal(3, 6, 0)
        hsSourceDirs' =  (map getSymbolicPath) . hsSourceDirs
#else
        hsSourceDirs' = hsSourceDirs
#endif

-- | Extract `Stanza`s from a package
packageStanzas :: PackageDescription -> [Stanza]
packageStanzas pd = let
  libStanza :: Library -> Stanza
  libStanza lib = mkStanza (libBuildInfo lib) (exposedModules lib) []
  exeStanza :: Executable -> Stanza
  exeStanza exe = mkStanza (buildInfo exe) [] [modulePath exe]
  testStanza :: TestSuite -> Stanza
  testStanza ts =
    mkStanza
      (testBuildInfo ts)
      (case testInterface ts of
         TestSuiteLibV09 _ mname -> [mname]
         _ -> [])
      (case testInterface ts of
         TestSuiteExeV10 _ path -> [path]
         _ -> [])
  benchStanza :: Benchmark -> Stanza
  benchStanza bn =
    mkStanza (benchmarkBuildInfo bn) [] $
    case benchmarkInterface bn of
      BenchmarkExeV10 _ path -> [path]
      _ -> []
  in mconcat
       [ maybeToList $ fmap libStanza $ library pd
       , fmap exeStanza $ executables pd
       , fmap testStanza $ testSuites pd
       , fmap benchStanza $ benchmarks pd
       ]

-- | Find cabal files that are "above" the source path
findCabalFiles :: FilePath -> FilePath -> IO (Maybe ([FilePath], FilePath))
findCabalFiles dir rel = do
  names <- getDirectoryContents dir
  cabalnames <-
    filterM (doesFileExist . (dir </>)) $ filter (isSuffixOf ".cabal") names
  case cabalnames of
    []
      | dir == "/" -> return Nothing
    [] -> findCabalFiles (takeDirectory dir) (takeFileName dir </> rel)
    _ -> return $ Just (fmap (\n -> dir </> n) cabalnames, rel)

getGenericPackageDescription :: FilePath -> IO (Maybe GenericPackageDescription)
#if MIN_VERSION_Cabal(2, 2, 0)
getGenericPackageDescription cabalPath = do
    cabaltext <- BS.readFile cabalPath
    return $ parseGenericPackageDescriptionMaybe cabaltext
#else
getGenericPackageDescription cabalPath = do
  cabaltext <- readFile cabalPath
  case parsePackageDescription cabaltext of
    ParseOk _ gpd -> return $ Just gpd
    _             -> return Nothing
#endif

-- | Find the `Stanza` that refers to this source path
getCabalStanza :: FilePath -> IO (Maybe Stanza)
getCabalStanza srcpath = do
  abssrcpath <- canonicalizePath srcpath
  mcp <- findCabalFiles (takeDirectory abssrcpath) (takeFileName abssrcpath)
  case mcp of
    Just (cabalpaths, relpath) -> do
      stanzass <-
        for cabalpaths $ \cabalpath -> do
          genericPackageDescription <- getGenericPackageDescription cabalpath
          case genericPackageDescription of
            Nothing -> return []
            Just gpd -> do
              return $ packageStanzas $ flattenPackageDescription gpd
      return $
        case filter (\stanza -> stanzaIsSourceFilePath stanza relpath) $
             mconcat stanzass of
          [] -> Nothing
          (stanza:_) -> Just stanza -- just pick the first one
    Nothing -> return Nothing

-- | Get (Cabal package) language and extensions from the cabal file for this source path
getCabalExtensions :: FilePath -> IO (Language, [Extension])
getCabalExtensions srcpath = do
  mstanza <- getCabalStanza srcpath
  return $
    case mstanza of
      Nothing -> (Haskell98, [])
      Just (MkStanza bi _) -> do
        (fromMaybe Haskell98 $ defaultLanguage bi, defaultExtensions bi)

convertLanguage :: Language -> GLP.Language
convertLanguage Haskell98 = GLP.Haskell98
convertLanguage Haskell2010 = GLP.Haskell2010
convertLanguage GHC2021 = GLP.GHC2021
convertLanguage (UnknownLanguage s) = error $ "Unknown language: " ++ s

-- | Get extensions from the cabal file for this source path
getCabalExtensionsForSourcePath :: FilePath -> IO [GLP.Extension]
getCabalExtensionsForSourcePath srcpath = do
  (lang, exts) <- getCabalExtensions srcpath
  let allExts = exts ++ implicitExtensions (convertLanguage lang)
  return $ uniqueExtensions $ concatMap extensionImplies allExts

uniqueExtensions :: [Extension] -> [GLP.Extension]
uniqueExtensions [] = []
uniqueExtensions ((EnableExtension e):xs) = convertExtension e:uniqueExtensions xs
uniqueExtensions ((DisableExtension e):xs) = uniqueExtensions $ filter (/= read (show e)) xs
uniqueExtensions ((UnknownExtension s):_) = error $ "Unknown extension: " ++ s

implicitExtensions :: GLP.Language -> [Extension]
implicitExtensions = fmap (EnableExtension . read . show) . languageExtensions . Just

extensionImplies :: Extension -> [Extension]
extensionImplies (EnableExtension e) = toExtension <$> filter (\(a, _, _) -> a == convertExtension e) impliedXFlags
    where toExtension (_, True, e') = EnableExtension $ read $ show e'
          toExtension (_, False, e') = DisableExtension $ read $ show e'
extensionImplies _ = []

-- `ghc-lib-parser`'s `Extension` does not implement `read`.
convertExtension :: KnownExtension -> GLP.Extension
convertExtension OverlappingInstances  = GLP.OverlappingInstances
convertExtension UndecidableInstances  = GLP.UndecidableInstances
convertExtension IncoherentInstances  = GLP.IncoherentInstances
convertExtension UndecidableSuperClasses  = GLP.UndecidableSuperClasses
convertExtension MonomorphismRestriction  = GLP.MonomorphismRestriction
convertExtension MonoLocalBinds  = GLP.MonoLocalBinds
convertExtension RelaxedPolyRec  = GLP.RelaxedPolyRec
convertExtension ExtendedDefaultRules  = GLP.ExtendedDefaultRules
convertExtension ForeignFunctionInterface  = GLP.ForeignFunctionInterface
convertExtension UnliftedFFITypes  = GLP.UnliftedFFITypes
convertExtension InterruptibleFFI  = GLP.InterruptibleFFI
convertExtension CApiFFI  = GLP.CApiFFI
convertExtension GHCForeignImportPrim  = GLP.GHCForeignImportPrim
convertExtension JavaScriptFFI  = GLP.JavaScriptFFI
convertExtension ParallelArrays  = GLP.ParallelArrays
convertExtension Arrows  = GLP.Arrows
convertExtension TemplateHaskell  = GLP.TemplateHaskell
convertExtension TemplateHaskellQuotes  = GLP.TemplateHaskellQuotes
convertExtension QualifiedDo  = GLP.QualifiedDo
convertExtension QuasiQuotes  = GLP.QuasiQuotes
convertExtension ImplicitParams  = GLP.ImplicitParams
convertExtension ImplicitPrelude  = GLP.ImplicitPrelude
convertExtension ScopedTypeVariables  = GLP.ScopedTypeVariables
convertExtension AllowAmbiguousTypes  = GLP.AllowAmbiguousTypes
convertExtension UnboxedTuples  = GLP.UnboxedTuples
convertExtension UnboxedSums  = GLP.UnboxedSums
convertExtension UnliftedNewtypes  = GLP.UnliftedNewtypes
convertExtension UnliftedDatatypes  = GLP.UnliftedDatatypes
convertExtension BangPatterns  = GLP.BangPatterns
convertExtension TypeFamilies  = GLP.TypeFamilies
convertExtension TypeFamilyDependencies  = GLP.TypeFamilyDependencies
convertExtension TypeInType  = GLP.TypeInType
convertExtension OverloadedStrings  = GLP.OverloadedStrings
convertExtension OverloadedLists  = GLP.OverloadedLists
convertExtension NumDecimals  = GLP.NumDecimals
convertExtension DisambiguateRecordFields  = GLP.DisambiguateRecordFields
convertExtension RecordWildCards  = GLP.RecordWildCards
convertExtension RecordPuns  = GLP.RecordPuns
convertExtension ViewPatterns  = GLP.ViewPatterns
convertExtension GADTs  = GLP.GADTs
convertExtension GADTSyntax  = GLP.GADTSyntax
convertExtension NPlusKPatterns  = GLP.NPlusKPatterns
convertExtension DoAndIfThenElse  = GLP.DoAndIfThenElse
convertExtension BlockArguments  = GLP.BlockArguments
convertExtension RebindableSyntax  = GLP.RebindableSyntax
convertExtension ConstraintKinds  = GLP.ConstraintKinds
convertExtension PolyKinds  = GLP.PolyKinds
convertExtension DataKinds  = GLP.DataKinds
convertExtension InstanceSigs  = GLP.InstanceSigs
convertExtension ApplicativeDo  = GLP.ApplicativeDo
convertExtension LinearTypes  = GLP.LinearTypes
convertExtension StandaloneDeriving  = GLP.StandaloneDeriving
convertExtension DeriveDataTypeable  = GLP.DeriveDataTypeable
convertExtension AutoDeriveTypeable  = GLP.AutoDeriveTypeable
convertExtension DeriveFunctor  = GLP.DeriveFunctor
convertExtension DeriveTraversable  = GLP.DeriveTraversable
convertExtension DeriveFoldable  = GLP.DeriveFoldable
convertExtension DeriveGeneric  = GLP.DeriveGeneric
convertExtension DefaultSignatures  = GLP.DefaultSignatures
convertExtension DeriveAnyClass  = GLP.DeriveAnyClass
convertExtension DeriveLift  = GLP.DeriveLift
convertExtension DerivingStrategies  = GLP.DerivingStrategies
convertExtension DerivingVia  = GLP.DerivingVia
convertExtension TypeSynonymInstances  = GLP.TypeSynonymInstances
convertExtension FlexibleContexts  = GLP.FlexibleContexts
convertExtension FlexibleInstances  = GLP.FlexibleInstances
convertExtension ConstrainedClassMethods  = GLP.ConstrainedClassMethods
convertExtension MultiParamTypeClasses  = GLP.MultiParamTypeClasses
convertExtension NullaryTypeClasses  = GLP.NullaryTypeClasses
convertExtension FunctionalDependencies  = GLP.FunctionalDependencies
convertExtension UnicodeSyntax  = GLP.UnicodeSyntax
convertExtension ExistentialQuantification  = GLP.ExistentialQuantification
convertExtension MagicHash  = GLP.MagicHash
convertExtension EmptyDataDecls  = GLP.EmptyDataDecls
convertExtension KindSignatures  = GLP.KindSignatures
convertExtension RoleAnnotations  = GLP.RoleAnnotations
convertExtension ParallelListComp  = GLP.ParallelListComp
convertExtension TransformListComp  = GLP.TransformListComp
convertExtension MonadComprehensions  = GLP.MonadComprehensions
convertExtension GeneralizedNewtypeDeriving  = GLP.GeneralizedNewtypeDeriving
convertExtension RecursiveDo  = GLP.RecursiveDo
convertExtension PostfixOperators  = GLP.PostfixOperators
convertExtension TupleSections  = GLP.TupleSections
convertExtension PatternGuards  = GLP.PatternGuards
convertExtension LiberalTypeSynonyms  = GLP.LiberalTypeSynonyms
convertExtension RankNTypes  = GLP.RankNTypes
convertExtension ImpredicativeTypes  = GLP.ImpredicativeTypes
convertExtension TypeOperators  = GLP.TypeOperators
convertExtension ExplicitNamespaces  = GLP.ExplicitNamespaces
convertExtension PackageImports  = GLP.PackageImports
convertExtension ExplicitForAll  = GLP.ExplicitForAll
convertExtension DatatypeContexts  = GLP.DatatypeContexts
convertExtension NondecreasingIndentation  = GLP.NondecreasingIndentation
convertExtension TraditionalRecordSyntax  = GLP.TraditionalRecordSyntax
convertExtension LambdaCase  = GLP.LambdaCase
convertExtension MultiWayIf  = GLP.MultiWayIf
convertExtension BinaryLiterals  = GLP.BinaryLiterals
convertExtension NegativeLiterals  = GLP.NegativeLiterals
convertExtension HexFloatLiterals  = GLP.HexFloatLiterals
convertExtension DuplicateRecordFields  = GLP.DuplicateRecordFields
convertExtension OverloadedLabels  = GLP.OverloadedLabels
convertExtension EmptyCase  = GLP.EmptyCase
convertExtension PatternSynonyms  = GLP.PatternSynonyms
convertExtension PartialTypeSignatures  = GLP.PartialTypeSignatures
convertExtension NamedWildCards  = GLP.NamedWildCards
convertExtension StaticPointers  = GLP.StaticPointers
convertExtension TypeApplications  = GLP.TypeApplications
convertExtension Strict  = GLP.Strict
convertExtension StrictData  = GLP.StrictData
convertExtension EmptyDataDeriving  = GLP.EmptyDataDeriving
convertExtension NumericUnderscores  = GLP.NumericUnderscores
convertExtension QuantifiedConstraints  = GLP.QuantifiedConstraints
convertExtension StarIsType  = GLP.StarIsType
convertExtension ImportQualifiedPost  = GLP.ImportQualifiedPost
convertExtension CUSKs  = GLP.CUSKs
convertExtension StandaloneKindSignatures  = GLP.StandaloneKindSignatures
convertExtension LexicalNegation  = GLP.LexicalNegation
convertExtension FieldSelectors  = GLP.FieldSelectors
convertExtension OverloadedRecordDot  = GLP.OverloadedRecordDot
convertExtension e = error $ "This extension is not supported by `ghc-lib-parser`: " ++ show e
