{-# LANGUAGE CPP #-}

module HIndent.CabalFile
  ( getCabalExtensionsForSourcePath
  ) where

import           Control.Monad
import qualified Data.ByteString                               as BS
import           Data.List
import           Data.Maybe
import           Data.Traversable
import           Distribution.ModuleName
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration
#if MIN_VERSION_Cabal(3, 6, 0)
import           Distribution.Utils.Path                       (getSymbolicPath)
#endif
#if MIN_VERSION_Cabal(2, 2, 0)
import           Distribution.PackageDescription.Parsec
#else
import           Distribution.PackageDescription.Parse
#endif
import           GHC.Driver.Session                            (impliedXFlags,
                                                                languageExtensions)
import qualified GHC.Driver.Session                            as GLP
import qualified GHC.LanguageExtensions.Type                   as GLP
import qualified HIndent.ExtensionConversion                   as EC
import           Language.Haskell.Extension
import           System.Directory
import           System.FilePath

data Stanza =
  MkStanza
    { _stanzaBuildInfo       :: BuildInfo
    , stanzaIsSourceFilePath :: FilePath -> Bool
    }

-- | Find the relative path of a child path in a parent, if it is a child
toRelative :: FilePath -> FilePath -> Maybe FilePath
toRelative parent child =
  let rel = makeRelative parent child
   in if rel == child
        then Nothing
        else Just rel

-- | Create a Stanza from `BuildInfo` and names of modules and paths
mkStanza :: BuildInfo -> [ModuleName] -> [FilePath] -> Stanza
mkStanza bi mnames fpaths =
  MkStanza bi $ \path ->
    let modpaths = fmap toFilePath $ otherModules bi ++ mnames
        inDir dir =
          case toRelative dir path of
            Nothing -> False
            Just relpath ->
              any (equalFilePath $ dropExtension relpath) modpaths ||
              any (equalFilePath relpath) fpaths
     in any inDir $ hsSourceDirs' bi
  where

#if MIN_VERSION_Cabal(3, 6, 0)
        hsSourceDirs' = (map getSymbolicPath) . hsSourceDirs
#else
        hsSourceDirs' = hsSourceDirs
#endif
-- | Extract `Stanza`s from a package
packageStanzas :: PackageDescription -> [Stanza]
packageStanzas pd =
  let libStanza :: Library -> Stanza
      libStanza lib = mkStanza (libBuildInfo lib) (exposedModules lib) []
      exeStanza :: Executable -> Stanza
      exeStanza exe = mkStanza (buildInfo exe) [] [modulePath exe]
      testStanza :: TestSuite -> Stanza
      testStanza ts =
        mkStanza
          (testBuildInfo ts)
          (case testInterface ts of
             TestSuiteLibV09 _ mname -> [mname]
             _                       -> [])
          (case testInterface ts of
             TestSuiteExeV10 _ path -> [path]
             _                      -> [])
      benchStanza :: Benchmark -> Stanza
      benchStanza bn =
        mkStanza (benchmarkBuildInfo bn) [] $
        case benchmarkInterface bn of
          BenchmarkExeV10 _ path -> [path]
          _                      -> []
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
          []         -> Nothing
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
convertLanguage Haskell98           = GLP.Haskell98
convertLanguage Haskell2010         = GLP.Haskell2010
#if MIN_VERSION_Cabal(3,6,0)
convertLanguage GHC2021             = GLP.GHC2021
#endif
convertLanguage (UnknownLanguage s) = error $ "Unknown language: " ++ s

-- | Get extensions from the cabal file for this source path
getCabalExtensionsForSourcePath :: FilePath -> IO [GLP.Extension]
getCabalExtensionsForSourcePath srcpath = do
  (lang, exts) <- getCabalExtensions srcpath
  let allExts = exts ++ implicitExtensions (convertLanguage lang)
  return $ EC.uniqueExtensions $ concatMap extensionImplies allExts

implicitExtensions :: GLP.Language -> [Extension]
implicitExtensions =
  fmap (EnableExtension . read . show) . languageExtensions . Just

extensionImplies :: Extension -> [Extension]
extensionImplies (EnableExtension e) =
  toExtension <$>
  filter (\(a, _, _) -> a == EC.convertExtension e) impliedXFlags
  where
    toExtension (_, True, e')  = EnableExtension $ read $ show e'
    toExtension (_, False, e') = DisableExtension $ read $ show e'
extensionImplies _ = []
